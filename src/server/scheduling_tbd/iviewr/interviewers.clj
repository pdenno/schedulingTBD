(ns scheduling-tbd.iviewr.interviewers
    "Runs an interview using an interview agent."
    (:require
     [cheshire.core                   :as ches]
     [clojure.core.unify              :as uni]
     [clojure.data.xml                :as xml]
     [clojure.pprint                  :refer [pprint]]
     [clojure.spec.alpha              :as s]
     [clojure.string                  :as str]
     [mount.core                      :as mount :refer [defstate]]
     [promesa.core                    :as p]
     [promesa.exec                    :as px]
     [scheduling-tbd.agent-db         :as adb :refer [agent-log]]
     [scheduling-tbd.db               :as db]
     [scheduling-tbd.llm              :as llm]
     [scheduling-tbd.iviewr.eads] ; for mount
     [scheduling-tbd.iviewr.eads-util      :as eads-util :refer [combine-ds!]]
     [scheduling-tbd.iviewr.ork            :as ork]
     [scheduling-tbd.iviewr.response-utils :as ru]
     [scheduling-tbd.sutil                 :as sutil :refer [elide output-struct2clj]]
     [scheduling-tbd.util                  :as util :refer [now]]
     [scheduling-tbd.web.websockets        :as ws]
     [scheduling-tbd.datastructure2mermaid :as ds2m]
     [taoensso.telemere                    :as tel :refer [log!]]))

;;; The usual way of interviews involves q-and-a called from resume-conversation.
;;; The alternative way, used only when the human or surrogate expert is starting, is to use get-an-answer.
;;; get-an-answer is called by q-and-a but is also is directly called by surrogate and human conversations on startup.
;;; The difference being that these two already know what the question is; it is the warm-up question of whatever
;;; conversation is running. The call to q-and-a, in contrast, uses SUPPLY-QUESTION to the interviewer.
;;;
;;; [resume-conversation] -> q-and-a -> get-an-answer -> chat-pair -> send-to-client or query-agent (sur).
;;; [start-up]                       -> get-an-answer -> ...

(def ^:diag diag (atom nil))
(s/def ::cid #(#{:process :data :resources :optimality} %))

;;; From the REPL you can change the active? atom to false anytime you want things to stop
;;; (like when it is burning OpenAI asking the same question over and over ;^)).
;;; If it doesn't stop, it is probably the case that you need to test for it in more places.
(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))

;;; For use of chat-pair and other things defined below.
(s/def ::chat-pair-ctx (s/and ::common-ctx
                              (s/or :surrogate ::surrogate-ctx :human ::human-ctx)))

(s/def ::common-ctx    (s/keys :req-un [::client-id ::question]))
(s/def ::surrogate-ctx (s/keys :req-un [:sur/responder-type ::surrogate-agent ::interviewer-agent]))
(s/def ::human-ctx     (s/keys :req-un [:hum/responder-type ::interviewer-agent]))
(s/def ::basic-agent (s/keys :req-un [::aid ::tid ::base-type]))
(s/def ::interviewer-agent ::basic-agent)
(s/def ::surrogate-agent ::basic-agent)
(s/def ::aid string?)
(s/def ::tid string?)
(s/def ::base-type keyword?)
(s/def :sur/responder-type #(= % :surrogate))
(s/def :hum/responder-type #(= % :human))
(s/def ::client-id (s/or :typical string? :debug #(= % :console)))
(s/def ::question string?)

;;; Optional
(s/def ::expert-response (s/keys :req-un [:expert/msg-type] :opt-un [:expert/text :expert/table :expert/table-html]))
(s/def :expert/msg-type #(= % :expert-response))
(s/def :expert/table map?)
(s/def :expert/table-html string?)
(s/def :expert/text string?)

(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate. This executes blocking.
   For human interactions, the response is based on :dispatch-key :domain-experts-says (See websockets/domain-expert-says)."
  [{:keys [question table table-text surrogate-agent responder-type preprocess-fn tries pid cid] :as ctx
    :or {tries 1  preprocess-fn identity}}]
  (log! :debug (str "ctx in chat-pair-aux:\n" (with-out-str (pprint ctx))))
   (let [prom (if (= :human responder-type)
                (ws/send-to-client (-> ctx                        ; This cannot timeout.
                                       (assoc :text question)
                                       (assoc :table table)
                                       (assoc :promise? true)
                                       (assoc :dispatch-key :iviewr-says)))
                (px/submit! (fn [] ; surrogate responder...
                              (try
                                (agent-log (str "["(name cid) " interviewer] (asking interviewees): " question "\n\n" table-text))
                                (adb/query-agent surrogate-agent  ; This can timeout.
                                                 (str question "\n\n" table-text)
                                                 {:tries tries
                                                  :base-type pid
                                                  :preprocess-fn preprocess-fn})
                                (catch Exception e {:error e})))))]
     (-> prom p/await)))

(declare separate-table)

(defn chat-pair
  "Call to run ask and wait for an answer.
   It returns a map {:msg-type :expert-response :full-text <string> :text <string> :table <table-map>} where
        :msg-type is either :expert-response or :immoderate.
        :full-text is text in response to the query (:question ctx), especially useful for use with surrogates.
        :text is text in response to the query with surrogate tables removed.
        :table is a table map completed by the expert.
   If response is immoderate returns {:msg-type :immoderate}"
  [{:keys [responder-type client-id cid] :as ctx}]
  (let [response (chat-pair-aux ctx)]
    (agent-log (str "["(name cid) " interviewer] (response from interviewees): " response))
    (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id})
    (case responder-type
      :human (if (-> response :text llm/immoderate?)
               {:msg-type :immoderate}
               response)
      ;; ToDo: Move the let up so can capture tables correctly with human too.
      :surrogate (let [{:keys [full-text text table-html table]} (separate-table response)]
                   (cond-> {:msg-type :expert-response :full-text full-text}
                     text  (assoc :text text)
                     table (assoc :table table)
                     table (assoc :table-html table-html))))))

;;; To check the structure of messages to and from the interviewer:
(s/def ::interviewer-msg (s/and (s/keys :req-un [:iviewr/message-type])
                                (fn [msg]
                                  (case (:message-type msg)
                                   :SUPPLY-QUESTION                   (s/valid? :iviewr/supply-question msg)
                                   :QUESTION-TO-ASK                   (s/valid? :iviewr/question-to-ask msg)
                                   :INTERVIEWEES-RESPOND              (s/valid? :iviewr/interviewees-respond msg)
                                   :DATA-STRUCTURE-REFINEMENT         (s/valid? :iviewr/data-structure-refinement msg)
                                   :CONVERSATION-HISTORY              (s/valid? :iviewr/conversation-history msg)
                                   :EADS-INSTRUCTIONS                 (s/valid? :iviewr/eads-instructions msg)
                                   :COURSE-CORRECTION                 (s/valid? :iviewr/course-correction msg)
                                   :STATUS                            #(string? (get % :status))))))

(s/def :iviewr/supply-question (s/keys :req-un [:iviewr/budget]))

(s/def :iviewr/question-to-ask (s/keys :req-un [:iviewr/question]))

(s/def :iviewr/interviewees-respond (s/keys :req-un [:iviewr/response]))
(s/def :iviewr/response string?)

(s/def :iviewr/data-structure-refinement (s/keys :req-un [:iviewr/commit-notes :iviewr/data-structure]))
(s/def :iviewr/commit-notes string?)
(s/def :iviewr/data-structure map?)

(s/def :iviewr/conversation-history (s/keys :req-un [:iviewr/budget :iviewr/activity :iviewr/interviewee-type] :opt-un [:iviewr/data-structure :iviewr/EADS]))
(s/def :iviewr/budget number?)
(s/def :iviewr/interviewee-type #(#{:human :machine} %))
(s/def :iviewr/activity (s/coll-of :iviewr/activity-map :kind vector?))
(s/def :iviewr/activity-map (s/keys :req-un [:iviewr/question :iviewr/answer]))
(s/def :iviewr/question string?)
(s/def :iviewr/answer string?)
(s/def :iviewr/data-structure map?)
(s/def :iviewr/EADS map?) ; ToDo: Tie in the EADSs?

(s/def :iviewr/eads-instructions (s/keys :req-un [:iviewr/interview-objective :iviewr/EADS]))
(s/def :iviewr/interview-objective string?)
(s/def :iviewr/EADS (s/or :dehydrated string? :hydrated map?))

(s/def :iviewr/course-correction (s/keys :opt-un [:iviewr/advice :iviewr/question]))

(def course-correction-count (atom 0))

(defn too-many-course-corrections!
  "If many course-corrections have occurred, stop the conversation."
  [{:keys [message]}]
  (when (= message :COURSE-CORRECTION)
    (swap! course-correction-count inc))
  (when (> @course-correction-count 5)
    (reset! active? false)))

(defn tell-interviewer
  "Send a message to an interviewer agent and wait for response; translate it.
   :aid and :tid in the opts should be for the interviewer agent."
  [msg {:keys [interviewer-agent cid] :as opts}]
  (reset! diag {:msg msg :opts opts})
  (too-many-course-corrections! msg)
  (when-not (s/valid? ::interviewer-msg msg) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid interviewer-msg:\n" (with-out-str (pprint msg)))))
  (agent-log (str "[interview manager] (tells " (name cid) " interviewer)\n" (with-out-str (pprint msg))))
  (log! :info (-> (str "Interviewer told: " msg) (elide 150)))
  (let [msg-string (ches/generate-string msg {:pretty true})
        res (-> (adb/query-agent interviewer-agent msg-string opts) output-struct2clj)
        res (cond-> res
              (contains? res :message-type)     (update :message-type keyword))]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    (agent-log (str "[interview manager] (receives from " (name cid) " interviewer)\n" (with-out-str (pprint res))))
    res))

(defn response-analysis
  "Return a map of booleans including
    - :answers-the-question? : which is true (some text) if the answer to the Q/A pair appears to answer the question.
    - :raises-a-question? : which is true (some text) if the response raises a question.
    - :wants-a-break?  : which is true (some text) if the user explicitly asks for a break?"
  [q-txt a-txt ctx]
  (assert (string? q-txt))
  (assert (string? a-txt))
  (let [res (-> (adb/query-agent :response-analysis-agent (format "QUESTION: %s \nRESPONSE: %s" q-txt a-txt) ctx)
                ches/parse-string
                (update-keys str/lower-case)
                (update-keys keyword)
                (update-vals #(if (empty? %) false %)))]
    (agent-log (str "[response analysis agent] (concludes):\n" (with-out-str (pprint res))))
    res))

;;; ToDo: Implement these (next three). They will probably involve some looping.
(defn handle-wants-a-break
  [_question response _ctx]
  (log! :warn (str "handle-wants-a-break not yet implemented: response = " response))
  [])

(defn handle-raises-a-question
  "The responder-type is :human. Handle a digression returning when the original-question is answered.
   Return a vector of the entire digression."
  [_question response _ctx]
  (log! :warn (str "handle-raises-a-question not yet implemented: response = " response))
  [])

(defn handle-other-non-responsive
  [_question response _ctx]
  (log! :warn (str "handle-other-non-responsive not yet implemented: response = " response))
  [])

(defn ask-again
  [question]
  (let [repeat-blurb "Okay, but we are still interested in this: "]
    (if (str/starts-with? question repeat-blurb) question (str repeat-blurb question))))

(defn get-an-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Return a vector of the conversation (objects suitable for db/add-msg) terminating in an answer to the question.
   Note that the first element of the conversation should be the question, and the last the answer.
   Might talk to the client to keep things moving if necessary. Importantly, this requires neither PID nor CID."
  [{:keys [question responder-type human-starting?] :as ctx}]
  (when-not human-starting?
    (when-not (s/valid? ::chat-pair-ctx ctx)
      (throw (ex-info "Invalid context in get-an-answer." {:ctx ctx}))))
  (let [conversation [{:text question :from :system :tags [:query]}]
        {:keys [full-text] :as response}  (chat-pair ctx) ; response here is text or a table (user either talking though chat or hits 'submit' on a table.
        answered? (= :surrogate responder-type) ; Surrogate doesn't beat around the bush, I think!
        {:keys [answers-the-question? raises-a-question? wants-a-break?]}  (when (and (not answered?) (string? full-text))
                                                                             (response-analysis question full-text ctx))
        answered? (or answered? answers-the-question?)]
    (if (not (or answered? wants-a-break? raises-a-question?))
      (let [same-question (ask-again question)]
        (into conversation (get-an-answer (-> ctx (assoc :question same-question)))))
      (cond-> conversation
        answered?                  (conj (-> response
                                             (assoc :from responder-type)
                                             (assoc :tags [:response])))
        wants-a-break?             (into (handle-wants-a-break question response ctx))
        raises-a-question?         (into (handle-raises-a-question question response ctx))
        (not
         (or answered?
             wants-a-break?
             raises-a-question?))  (into (handle-other-non-responsive question response ctx))))))

(defn surrogate? [pid] (ru/find-claim '(surrogate ?x) (db/get-claims pid)))

;;; ToDo: It isn't obvious that the interview agent needs to see all this. Only useful when the thread was destroyed?
;;; ToDo: V4 instructions do not mention claims, but this still holds
(defn conversation-history
  "Construct a conversation-history message structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked EXCEPT for the warm-up question (if any) which happens before the interview."
  [pid cid]
  (let [answers (->> cid
                     (db/get-conversation pid)
                     :conversation/messages
                     (filter #(contains? % :message/answers-question)))]
    (cond-> {:message-type :CONVERSATION-HISTORY
             :interviewee-type (if (surrogate? pid) :machine :human)
             :budget (db/get-budget pid cid)}
      (not-empty answers)   (assoc :activity (mapv (fn [answer]
                                                     {:question (-> (db/get-msg pid cid (:message/answers-question answer)) :message/content)
                                                      :answer  (-> answer :message/content)}) ; ToDo: Is this inclusive of tables?
                                                   answers)))))

(defn make-supply-question-msg
  "Create a supply question message for the conversation."
  [{:keys [pid cid] :as _ctx}]
  {:message-type :SUPPLY-QUESTION :budget (db/get-budget pid cid)})

(defn fix-off-course--question
  "Generate and communicate a COURSE-CORRECTION message based on the argument question."
  [q _ctx] q) ; ToDo: NYI

;;; --------------- Handle tables from interviewer --------------------------------------
(defn table-xml2clj
  "Remove some useless aspects of the parsed XHTML, including string content and attrs."
  [table-xml]
  (letfn [(x2c [x]
            (cond (seq? x) (mapv x2c x)
                  (map? x) (reduce-kv (fn [m k v]
                                        (cond (= k :content)        (assoc m k (->> v (remove #(and (string? %) (re-matches #"^\s+" %))) vec x2c))
                                              (= k :attrs)          m ; There aren't any values here in our application.
                                              :else                 (assoc m k (x2c v))))
                                      {}
                                      x)
                  (vector? x) (mapv x2c x)
                  :else x))]
    (x2c table-xml)))

(defn table2obj
  "Convert the ugly XML-like object (has :tr :th :td) to an object with :table-headings and :table-data, where
  (1) :table-headings is a vector of maps with :title and :key and,
  (2) :table-data is a vector of maps with keys that are the :key values of (1)
  These :key values are :title as a keyword. For example:

   ugly-table:

  {:tag :table,
   :content [{:tag :tr, :content [{:tag :th, :content ['Dessert (100g serving)']} {:tag :th, :content ['Carbs (g)']} {:tag :th, :content ['Protein (g)']}]}
             {:tag :tr, :content [{:tag :td, :content ['frozen yogurt']} {:tag :td, :content [24]}  {:tag :td, :content [4.0]}]}
             {:tag :tr, :content [{:tag :td, :content ['ice cream']}     {:tag :td, :content [37]}  {:tag :td, :content [4.3]}]}]}

  Would result in:
  {:table-headings [{:title 'Dessert (100g serving)', :key :dessert}
                    {:title 'Carbs (g)',              :key :carbs}
                    {:title 'Protein (g)',            :key :protein}]
   :table-data [{:dessert 'frozen yogurt', :carbs 24 :protein 4.0}
                {:dessert 'ice cream',     :carbs 37 :protein 4.3}]}."
  [ugly-table]
  (let [heading (-> ugly-table :content first :content)
        titles (if (every? #(= (:tag %) :th) heading)
                 (->> heading
                      (mapv #(-> % :content first))
                      (mapv #(-> {} (assoc :title %) (assoc :key (-> % ru/text-to-var keyword {:asking-role :table2obj})))))
                 (log! :warn "No titles in ugly table."))
        title-keys (map :key titles)
        data-rows  (->> ugly-table
                        :content                                 ; table content
                        rest                                     ; everything but header
                        (mapv (fn [row]
                                (mapv #(or (-> % :content first) "") (:content row)))))]
    {:table-headings  titles
     :table-data (mapv (fn [row] (zipmap title-keys row)) data-rows)}))

(defn separate-table-aux
  "Look through the text (typically an interviewer question) for '#+begin_src HTML ... #+end_src'; what is between those markers should be a table.
   Return a map where
      :full-text is the argument text,
      :text is the argument string minus the table, and
      :table-html is the substring between the markers."
  [text]
  (let [in-table? (atom false)]
    (loop [lines (str/split-lines text) ; <=======================================
           res {:full-text text :text "" :table-html ""}]
      (let [l (first lines)]
        (when (and l @in-table? (re-matches #"(?i)^\s*\#\+end_src\s*" l))  (reset! in-table? false))
        (when (and l (re-matches #"(?i)^\s*\#\+begin_src\s+HTML\s*" l))    (reset! in-table? true))
        (if (empty? lines)
          res
          (recur (rest lines)
                 (cond (re-matches #"(?i)^\s*\#\+begin_src\s+HTML\s*" l) res
                       (re-matches #"(?i)^\s*\#\+end_src\s*"          l) res
                       (not @in-table?) (update res :text       #(str % "\n" l))
                       @in-table?       (update res :table-html #(str % "\n" l)))))))))

(defn separate-table
  "Arg may be (1) a string that might contain an embedded table, or (2) a context map that contains :question.
   Return a map containing :full-text (a string) :text (a string) :table-html (a string)  and :table (a map).
   Where :text is a substring of :full-text (argument text)."
  [arg]
  (let [text (if (string? arg) arg (:question arg))
        {:keys [table-text] :as res} (separate-table-aux text)]
    (if (not-empty table-text)
      (try (as-> res ?r
             (assoc ?r :table (-> ?r :table-html java.io.StringReader. xml/parse table-xml2clj table2obj))
             (assoc ?r :status :ok))
           ;; ToDo:  This catch is not catching!?!
           (catch Throwable _e (assoc res :status :invalid-table)))
      res)))

(s/def ::q-and-a (s/keys :req-un [::question ::client-id]))
(s/def ::question string?)

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call q-and-a
;;; over and over and the interview will advance each time.
(defn q-and-a
  "Call interviewer to supply a question; call get-an-answer for an answer prefaced by zero or more messages non-responsive to the question.
   Returns a vector of message objects suitable for db/add-msg (contains :pid :cid :from, :text :table...).
   The first element in the vector should be the question object, and the last the answer object."
  [{:keys [client-id responder-type] :as ctx}]
  (ws/send-to-client {:dispatch-key :interviewer-busy? :value true :client-id client-id})
  (try (let [iviewr-q (-> (make-supply-question-msg ctx) ; This just makes a SUPPLY-QUESTION message-type.
                          (tell-interviewer ctx)         ; Returns (from the interviewer) a {:message-type :QUESTION-TO-ASK, :question "...'}
                          (separate-table)               ; Returns a {:text "..." :table-html "..." :table {:table-headings ... :tabel-data ...}}
                          (fix-off-course--question ctx))]         ; Currently a no-op. Returns argument.
         (when (= :human responder-type) (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id}))
         ;; This returns a VECTOR of statements from the interviewees and interviewer.
         (get-an-answer (cond-> ctx
                          true                                (assoc :question (:text iviewr-q))
                          (-> iviewr-q :table-html not-empty) (assoc :table-html (:table-html iviewr-q)))))
       (finally (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;; ToDo: This needs work. You should be able to reopen a conversation at any time.
(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (letfn [(done? [status] (= status :eads-exhausted))]
    (let [[pstat dstat rstat _ostat]  (doall (for [c [:process :data :resources :optimality]]
                                              (db/get-conversation-status pid c)))]
      (case cid
            :resources               (and (done? pstat) (done? dstat))
            :optimality              (and (done? pstat) (done? dstat) (done? rstat))
            :data                    (not (done? dstat))
            :process                 (not (done? pstat))))))

;;; ToDo: This needs work. You should be able to reopen a conversation at any time. Maybe need an agent for this!
(defn find-appropriate-conv-and-redirect
  "If we think the conversation is over, ask them if they want to reopen it.
  Put a message in the current chat to go to the recommended chat or
   Typically this is called with ready-for-discussion? returned false."
  [{:keys [pid cid client-id]}]
  (letfn [(ready? [status] (#{:not-started :in-progress} status))]
    (let [[pstat dstat rstat ostat]  (doall (for [c [:process :data :resources :optimality]]
                                              (db/get-conversation-status pid c)))
          better-choice (case cid
                          :process      (cond (ready? dstat) :data
                                              (ready? rstat) :resources
                                              (ready? ostat) :optimality
                                              :else          :all-done!)
                          :data         (cond (ready? pstat) :process
                                              (ready? rstat) :resources
                                              (ready? ostat) :optimality
                                              :else          :all-done!)
                          :resources    (cond (ready? pstat) :process
                                              (ready? dstat) :data
                                              (ready? ostat) :optimality
                                              :else          :all-done!))
          text (if (= :all-done! better-choice)
                 "We are satisfied with this conversation as it is. Did you want to reopen it for discussion?"
                 (str "At this point in our work, it would be better to discuss " (name better-choice) ". "
                      "Could you select " (-> better-choice name str/capitalize) " from the menu on the left?"))]
      ;; Note that you don't send cid; it lands in whatever conversation the users is looking at.
      (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :text text}))))

(defn ctx-surrogate
  "Return context updated with surrogate info."
  [{:keys [pid cid force-new?] :as ctx}]
  (let [interviewer-agent (adb/ensure-agent! {:base-type (-> cid name (str "-interview-agent") keyword) :pid pid})
        surrogate-agent   (adb/ensure-agent! {:base-type pid :pid pid} {:make-agent? force-new?})
        ctx (-> ctx
                (assoc :responder-type :surrogate)
                (assoc :interviewer-agent interviewer-agent)
                (assoc :surrogate-agent surrogate-agent))]
    (if (s/valid? ::surrogate-ctx ctx)
      ctx
      (do (reset! diag ctx)
          (throw (ex-info "Invalid surrogate context:" {:ctx ctx}))))))

(defn ctx-human
  "Return  part of context specific to humans."
  [{:keys [pid cid]}]
   {:responder-type :human
    :interviewer-agent (adb/ensure-agent! {:base-type (-> cid name (str "-interview-agent") keyword) :pid pid})})

(defn start-human-project!
  "This does a few things to 'catch up' with the surrogate-mode of operation, which alreadys knows what the project is about.
      1) Analyze the response from the :process/warm-up question.
          a) make a project (now you know the real pid).
          b) add scheduling challenge claims.
      2) Add the process-interview-agent.
   Return pid of new project."
  [response]
  (let [warm-up-claims (ru/analyze-warm-up :process response) ; Projects start with the process conversation.
        [_ pid] (ru/find-claim '(temp-project-id ?pid) warm-up-claims)
        [_ _ pname] (ru/find-claim '(temp-project-name ?pid ?pname) warm-up-claims)
        pid (db/create-proj-db! {:project/id pid :project/name pname})
        bindings {'?pid pid}
        needed-claims (remove #('#{temp-project-id temp-project-name} (first %)) warm-up-claims)]
      (doseq [claim needed-claims]
        (db/add-claim! pid {:string (-> claim (uni/subst bindings) str)
                            :q-type :process/warm-up
                            :cid :process}))
      (adb/ensure-agent! {:base-type :orchestrator-agent :pid pid})
      pid))

(defn put-EADS-ds-on-msg!
  "Clean-up the EADS data structure and save it on a property :message/EADS-data-structure
   of the interviewee response which is responsible for this update (the current response
   in the argument conversation)."
  [pid cid iviewr-response]
  (letfn [(up-keys [obj]
            (cond (map? obj)     (reduce-kv (fn [m k v] (assoc m (keyword k) (up-keys v))) {} obj)
                  (vector? obj)  (mapv up-keys obj)
                  :else          obj))]
    (let [obj (-> iviewr-response
                  up-keys
                  (update :message-type keyword)
                  (update :EADS-ref keyword))]
      (db/put-EADS-ds! pid cid obj))))

(defn update-db-conversation!
  "Write to the project DB messages that occurred inclusive of a q/a pair.
   If there is a EADS being pursued, set the pursuing-EADS.
   Mark the last message as answering the first.
   Argument msgs-vec is a vector of maps having (possibly) :from :text :table :tags, and :question-type."
  [pid cid msgs-vec]
  (reset! diag msgs-vec)
  (let [msg-ids (atom [])
        EADS-id (db/get-active-EADS-id pid cid)]
    (doseq [msg msgs-vec]
      (swap! msg-ids conj (db/add-msg (cond-> (merge {:pid pid :cid cid} msg)
                                        EADS-id  (assoc :pursuing-EADS EADS-id)))))
    (db/update-msg pid cid (last @msg-ids) {:message/answers-question (first @msg-ids)})))

(defn post-ui-actions
  "Send client actions to perform such as loading graphs or (for humans) changing conversations."
  [iviewr-response {:keys [pid cid new-cid? client-id responder-type] :as _ctx}]
  (when (surrogate? pid) (ru/refresh-client client-id pid cid))
  (when (= :DATA-STRUCTURE-REFINEMENT (:message-type iviewr-response))
    ;; Using the data structure being pursued in some EADS instructions to show a FFBD of their processes.
    (when-let [ds-id (#{:process/flow-shop :process/job-shop--classifiable} (db/get-active-EADS-id pid cid))]
      (let [graph-mermaid (ds2m/ds2mermaid pid ds-id)]
        (ws/send-to-client {:client-id client-id :dispatch-key :load-graph :graph graph-mermaid}))))
  ;; Of course, we don't switch the conversation for humans; we have to tell them to switch.
  (when (and new-cid? (= :human responder-type))
    (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :promise? true
                        :text (str "Right now we don't have much more to ask on this conversation, "
                                   "but we'd like to continue on the " (-> cid name str/capitalize) "conversation. "
                                   "Would you mind switching over there? (Use menu on the right.)")})))

(defn post-db-actions!
  "If the interviewer responded with a DATA-STRUCTURE-REFINEMENT (which is typical), associate the
   whole message as a string to the message which is an answer to the question."
  [iviewr-response {:keys [pid cid]}]
  (when (= :DATA-STRUCTURE-REFINEMENT (:message-type iviewr-response))
    (put-EADS-ds-on-msg! pid cid iviewr-response)
    (when-let [eads-id (db/get-active-EADS-id pid cid)]
      (combine-ds! eads-id pid))))

(defn clear-ctx-ephemeral
  "Some properties in the context map are only relevant in the current iteration of the interview loop.
   Those must be cleared before the next iteration. This function returns the argument ctx with those props removed."
  [ctx]
  (dissoc ctx :new-eads-id :new-cid? :old-cid))

;;; (inv/ork-review {:pid :plate-glass-ork :cid :process})
(defn ork-review
  "Looks at the project DB to decide whether there is an active-eads-id and whether the active is complete.
   If there is no active-eads-id or the active one is complete, ork will get a conversation-history (including completed EADS data structures,
   and will determine a new-eads-id or return nil (if conversation for EADS-ds has been exhausted).
   The consequences of an ork-review can be:
         1) a change in the eads to pursue (because either there is none yet, the current on is complete, or exceeded budget),
         2) a change in interviewer, which occurs simply by changing :cid in the context; no initialization needed.
         3) a message to human interviewees to change conversations to what the ork wants to do,
         4) interviewing to stop (because ork conludes that no more eads-instructions apply, or forced by active? atom)."
  [pid cid]
  (ork/ensure-ork! pid)
  (let [budget-ok? (> (db/get-budget pid cid) 0)
        active-eads-id (db/get-active-EADS-id pid cid)
        active-eads-complete? (or (not budget-ok?)
                                  (and active-eads-id (eads-util/ds-complete? active-eads-id pid)))
        new-eads-id (when (or active-eads-complete? (not active-eads-id))
                      (ork/get-new-EADS-id pid))
        change-eads? new-eads-id
        eads-id (or new-eads-id active-eads-id)
        exhausted? (not eads-id)
        new-cid (when new-eads-id
                  (let [nspace (when eads-id (-> eads-id namespace keyword))]
                    (when (not= cid nspace) nspace)))]
    (when-not budget-ok?
      (agent-log (str "[ork manager] (in ork-review) switching to new EADS instructions " new-eads-id "  owing to budget.")
                 {:console? true :level :warn}))
    (when new-cid
      (agent-log (str "[ork manager] (in ork-review) changing conversation to " new-cid ".")))
    (let [res (cond-> {}
                (not @active?)                    (assoc :force-stop? true)
                exhausted?                        (assoc :exhausted? true)
                change-eads?                      (assoc :change-eads? true)
                new-eads-id                       (assoc :new-eads-id new-eads-id)
                new-cid                           (assoc :new-cid? true :old-cid cid :cid new-cid)
                (not new-cid)                     (assoc :cid cid))]
      (agent-log (str "[ork manager] (in ork-review) returning decision (has extra info):\n"
                      (with-out-str (pprint res)))
                 {:console? true})
      res)))

;;; resume-conversation is typically called by client dispatch :resume-conversation.
;;; start-conversation can make it happen by asking the client to load-project (with cid = :process).
;;; ToDo: Will need to keep track in the DB of the interviewer tid on which each element of conversation happened.
;;;       Use this to decide what to put into CONVERSATION-HISTORY messages.
;;; (inv/resume-conversation {:client-id :console :pid :sur-music-school :cid :process})
(defn resume-conversation
  "Resume the interview loop for an established project and given cid."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (s/valid? ::client-id client-id))
  (assert (s/valid? ::cid cid))
  (reset! course-correction-count 0)
  (agent-log (str "============= resume-conversation: " pid  " " cid " " (now) " ========================")
             {:console? true :level :debug})
  (if (ready-for-discussion? pid cid)
    (try
      (let [ctx (if (surrogate? pid) (merge ctx (ctx-surrogate ctx)) (merge ctx (ctx-human ctx)))]
        (-> (conversation-history pid cid) (tell-interviewer ctx))
        (loop [ctx ctx] ; ToDo: currently not used!
          (let [{:keys [force-stop? exhausted? change-eads? new-eads-id cid old-cid new-cid?]} (ork-review pid cid)]
            (if (or exhausted? force-stop?)
              (agent-log (str "resume-conversation exiting. ctx:\n" (with-out-str (pprint ctx))) {:console? true})
              (do (when new-cid?
                    (db/put-conversation-status! pid old-cid :eads-exhausted)
                    (db/put-conversation-status! pid cid :in-progress))
                  (when change-eads?
                    (log! :warn (str "Changing EADS, new-eads-id = " new-eads-id))
                    (db/put-active-EADS-id pid cid new-eads-id) ; This will be used by :message/pursuing-EADS.
                    (tell-interviewer (db/get-EADS-instructions new-eads-id) ctx))
                  (let [conversation (q-and-a ctx) ; q-and-a does a SUPPLY-QUESTION and returns a vec of msg objects suitable for db/add-msg.
                        expert-response (-> conversation last :full-text) ; ToDo: This assumes the last is meaningful.
                        ;; Expect a DATA-STRUCTURE-REFINEMENT message to be returned from this call.
                        iviewr-response (tell-interviewer {:message-type :INTERVIEWEES-RESPOND :response expert-response}
                                                          {:interviewer-agent (:interviewer-agent ctx)
                                                           :cid cid
                                                           :tries 2
                                                           :test-fn output-struct2clj})]
                    (db/put-budget! pid cid (- (db/get-budget pid cid) 0.05))
                    (update-db-conversation! pid cid conversation)
                    (post-ui-actions  iviewr-response ctx)  ; show graphs and tables, tell humans to switch conv.
                    (post-db-actions! iviewr-response ctx)) ; associate DS refinement with msg.
                  (recur (clear-ctx-ephemeral ctx)))))))
      (finally (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id})))
    (find-appropriate-conv-and-redirect ctx)))

;;; The equivalent for surrogates is start-surrogate. It also asks the first question.
(defn start-conversation
  "Ask a human the first question (a warm-up question), create a project and call resume-conversation.
   :start-conversation is a dispatch-key from the client, providing, perhaps only the client-id."
  [{:keys [client-id cid] :as ctx}]
  (assert (= :process cid)) ; Conversation always starts with the :process conversation.
  (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :promise? true
                      :text (str "You want to start a new project? This is the right place! "
                                 (db/conversation-intros :process))})
  (try
    (let [ctx (merge ctx {:responder-type :human
                          ;; There is no need to subsequently clear :human-starting because we'll continue the
                          ;; conversation with resume-conversation, which only gets pid and cid.
                          :human-starting? true
                          :question (ru/get-warm-up-q cid)})
          conversation (get-an-answer ctx) ; get-an-answer also used by q-and-a, but here we have the q.
          response (-> conversation last :text)
          pid (start-human-project! response)
          [_ _ pname] (ru/find-claim '(project-name ?pid ?pname) (db/get-claims pid))]
      (ws/send-to-client {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (doseq [{:keys [from text table tags]} conversation]
        (db/add-msg {:pid pid :cid :process :from from :tags tags :text text :table table}))
      (db/add-msg {:pid pid
                   :cid :process
                   :from :system
                   :text (str "Great, we'll call your project " pname ".")
                   :tags [:process/warm-up :name-project :informative]})
      ;; This will cause a resume-conversation:
      (ws/send-to-client {:dispatch-key :load-proj :client-id client-id  :promise? false
                          :new-proj-map {:project/name pname :project/id pid}}))
    (finally
      (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :start-conversation start-conversation)
  (ws/register-ws-dispatch :resume-conversation resume-conversation))

(defstate iviewrs
  :start (init-iviewers!))
