(ns scheduling-tbd.interviewing.interviewers
    "Runs an interview uing an interview agent."
    (:require
     [clojure.core.unify            :as uni]
     [clojure.data.xml              :as xml]
     [clojure.edn                   :as edn]
     [clojure.pprint                :refer [pprint cl-format]]
     [clojure.java.io               :as io]
     [clojure.spec.alpha            :as s]
     [clojure.string                :as str]
     [jsonista.core                 :as json]
     [mount.core                    :as mount :refer [defstate]]
     [promesa.core                  :as p]
     [promesa.exec                  :as px]
     [scheduling-tbd.agent-db       :as adb :refer [agent-log]]
     [scheduling-tbd.db             :as db]
     [scheduling-tbd.interviewing.domain.data-analysis :as dan]
     [scheduling-tbd.interviewing.domain.process-analysis :as pan :refer [the-warm-up-type-question]]
     [scheduling-tbd.interviewing.domain.optimality-analysis]
     [scheduling-tbd.interviewing.domain.resources-analysis]
     [scheduling-tbd.llm            :as llm]
     [scheduling-tbd.interviewing.response-utils :as ru :refer [find-claim]]
     [scheduling-tbd.sutil          :as sutil :refer [elide output-struct2clj]]
     [scheduling-tbd.web.websockets :as ws]
     [taoensso.telemere             :as tel :refer [log!]]))

(def ^:diag diag (atom nil))
(s/def ::cid #(#{:process :data :resources :optimality} %))

;;; From the REPL you can change the active? atom to false anytime you want things to stop
;;; (like when it is burning OpenAI asking the same question over and over ;^)).
;;; If it doesn't stop, it is probably the case that you need to test for it in more places.
(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))

;;; For use of chat-pair and other things defined below.
(s/def ::chat-pair-ctx (s/and ::common-ctx
                              (s/or :surrogate (s/and #(s/valid? ::surrogate-ctx %) #(s/valid? ::common-ctx %))
                                    :human     (s/and #(s/valid? ::human-ctx %)     #(s/valid? ::common-ctx %)))))

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
(s/def ::client-id string?)
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
  [{:keys [question table table-text surrogate-agent responder-type preprocess-fn tries asking-role pid cid] :as ctx
    :or {tries 1  preprocess-fn identity}}]
  (log! :debug (str "ctx in chat-pair-aux: " (with-out-str (pprint ctx))))
   (let [asking-role (or asking-role (if cid (-> (str cid "-interviewer") keyword) :an-interviewer))
         prom (if (= :human responder-type)
                (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                     (assoc :text question)
                                     (assoc :table table)
                                     (assoc :promise? true)
                                     (assoc :dispatch-key :iviewr-says)))
                (px/submit! (fn [] ; surrogate responder...
                              (try
                                (adb/query-agent surrogate-agent  ; This can timeout.
                                                 (str question "\n\n" table-text)
                                                 {:tries tries
                                                  :asking-role asking-role
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
  [{:keys [responder-type client-id] :as ctx}]
  (let [response (chat-pair-aux ctx)]
    (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id})
    (case responder-type
      :human (if (-> response :text llm/immoderate?)
               {:msg-type :immoderate}
               response)
      :surrogate (let [{:keys [full-text text table-html table]} (separate-table response)]
                   (cond-> {:msg-type :expert-response :full-text full-text}
                     text  (assoc :text text)
                     table (assoc :table table)
                     table (assoc :table-html table-html))))))

;;; To check the structure of messages to and from the interviewer:
(s/def ::interviewer-msg (s/and (s/keys :req-un [:iviewr/message-type])
                                (fn [msg]
                                  (case (-> msg :message-type name)
                                   "SUPPLY-QUESTION"                   (s/valid? :iviewr/supply-question msg)
                                   "QUESTION-TO-ASK"                   (s/valid? :iviewr/question-to-ask msg)
                                   "INTERVIEWEES-RESPOND"              (s/valid? :iviewr/interviewees-respond msg)
                                   "DATA-STRUCTURE-REFINEMENT"         (s/valid? :iviewr/data-structure-refinement msg)
                                   "CONVERSATION-HISTORY"              (s/valid? :iviewr/conversation-history msg)
                                   "EADS"                              (s/valid? :iviewr/eads-msg msg)
                                   "COURSE-CORRECTION"                 (s/valid? :iviewr/course-correction msg)
                                   "STATUS"                            #(string? (get % :status))))))

(s/def :iviewr/supply-question (s/keys :req-un [:iviewr/budget]))

(s/def :iviewr/question-to-ask (s/keys :req-un [:iviewr/question]))

(s/def :iviewr/interviewees-respond (s/keys :req-un [:iviewr/response]))
(s/def :iviewr/response string?)

(s/def :iviewr/data-structure-refinement (s/keys :req-un [:iviewr/commit-notes :iviewr/data-structure]))
(s/def :iviewr/commit-notes string?)
(s/def :iviewr/data-structure map?)

(s/def :iviewr/conversation-history (s/keys :req-un [:iviewr/budget :iviewr/Q-A-pairs :iviewr/interviewee-type] :opt-un [:iviewr/data-structure :iviewr/EADS]))
(s/def :iviewr/budget number?)
(s/def :iviewr/interviewee-type #(#{:human :machine} %))
(s/def :iviewr/Q-A-pairs (s/coll-of :iviewr/q-a-map :kind vector?))
(s/def :iviewr/q-a-map (s/keys :req-un [:iviewr/question :iviewr/answer]))
(s/def :iviewr/question string?)
(s/def :iviewr/answer string?)
(s/def :iviewr/data-structure map?)
(s/def :iviewr/EADS map?) ; ToDo: Tie in the EADSs?

(s/def :iviewr/eads-msg (s/keys :req-un [:iviewr/interview-objective :iviewr/EADS]))
(s/def :iviewr/interview-objective string?)
(s/def :iviewr/EADS string?)

(s/def :iviewr/course-correction (s/keys :opt-un [:iviewr/advice :iviewr/question]))

(def course-correction-count (atom 0))

(defn too-many-course-corrections!
  "If many course-corrections have occurred, stop the conversation."
  [{:keys [message]}]
  (when (= message "COURSE-CORRECTION")
    (swap! course-correction-count inc))
  (when (> @course-correction-count 5)
    (reset! active? false)))

(defn tell-interviewer
  "Send a message to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [msg {:keys [interviewer-agent] :as ctx}]
  (too-many-course-corrections! msg)
  (when-not (s/valid? ::interviewer-msg msg) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid interviewer-msg: " (with-out-str (pprint msg)))))
  (log! :info (-> (str "Interviewer told: " msg) (elide 150)))
  (let [msg-string (json/write-value-as-string msg)
        res (-> (adb/query-agent interviewer-agent msg-string ctx) output-struct2clj)]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    res))

(defn response-analysis
  "Return a map of booleans including
    - :answers-the-question? : which is true (some text) if the answer to the Q/A pair appears to answer the question.
    - :raises-a-question? : which is true (some text) if the response raises a question.
    - :wants-a-break?  : which is true (some text) if the user explicitly asks for a break?"
  [q-txt a-txt ctx]
  (assert (string? q-txt))
  (assert (string? a-txt))
  (-> (adb/query-agent :response-analysis-agent (format "QUESTION: %s \nRESPONSE: %s" q-txt a-txt) ctx)
      json/read-value
      (update-keys str/lower-case)
      (update-keys keyword)
      (update-vals #(if (empty? %) false %))))

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

;;; ToDo: The design question I still have here is how to get back on track when the user's response doesn't answer the question.
;;;       This is mostly a matter for handle-raises-a-question ...(Thread/sleep 20000).
;;; 2025-03-06 I am reverting to this version because the one Nicolas pushed isn't working for me. (That one is commented out below.)
#_(defn get-an-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Return a vector of the conversation (objects suitable for db/add-msg) terminating in an answer to the question.
   Might talk to the client to keep things moving if necessary. Importantly, this requires neither PID nor CID."
  [{:keys [question responder-type human-starting?] :as ctx}]
  (when-not human-starting? (s/assert ::chat-pair-ctx ctx))
  (let [conversation [{:text question :from :system :tags [:query]}]
        {:keys [full-text] :as response}  (chat-pair ctx) ; response here is text or a table (user either talking though chat or hits 'submit' on a table.
        answered? (= :surrogate responder-type) ; Surrogate doesn't beat around the bush, I think!
        {:keys [answers-the-question? raises-a-question? wants-a-break?]}  (when (and (not answered?) (string? full-text))
                                                                             (response-analysis question full-text ctx))
        answered? (or answered? answers-the-question?)]
    (if (not (or answered? wants-a-break? raises-a-question?))
      (let [same-question (str "Okay, but we are still interested in this: " question)]
        (into conversation (get-an-answer (assoc ctx :question same-question))))
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

(defn ask-again
  [question]
  (let [repeat-blurb "Okay, but we are still interested in this: "]
    (if (str/starts-with? question repeat-blurb) question (str repeat-blurb question))))

(defn get-an-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Return a vector of the conversation (objects suitable for db/add-msg) terminating in an answer to the question.
   Might talk to the client to keep things moving if necessary. Importantly, this requires neither PID nor CID."
  [{:keys [question responder-type human-starting?] :as ctx}]
  (when-not human-starting? (s/assert ::chat-pair-ctx ctx))
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
;;;       It might be sufficient to explain the claims. ToDo: V4 doesn't have claims, but this still holds
(defn conversation-history
  "Construct a conversation-history message structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked EXCEPT for the warm-up question (if any) which happens before the interview."
  [pid cid]
  (let [msgs (-> (db/get-conversation pid cid) :conversation/messages)
        questions (filter #(= :system (:message/from %)) msgs)] ; Some aren't questions; we deal with it.
    (cond-> {:message-type "CONVERSATION-HISTORY"
             :interviewee-type (if (surrogate? pid) :machine :human)
             :budget (db/get-budget pid cid)
             :Q-A-pairs (->> questions
                             (map (fn [q]
                                    (let [next-id (inc (:message/id q))]
                                      (as-> {} ?pair
                                        (assoc ?pair :question (:message/content q))
                                        (if-let [response (when-let [resp (some #(when (= next-id (:message/id %)) %) msgs)]
                                                            (when (#{:human :surrogate} (:message/from resp)) resp))]
                                          (assoc ?pair :answer (:message/content response))
                                          ?pair)))))
                             (filter #(contains? % :answer))
                             vec)})))

(defn make-supply-question-msg
  "Create a supply question message for the conversation."
  [{:keys [pid cid] :as _ctx}]
  {:message-type "SUPPLY-QUESTION" :budget (db/get-budget pid cid)})

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
    (loop [lines (str/split-lines text)
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
   Returns a vector of message objects suitable for db/add-msg."
  [{:keys [client-id responder-type] :as ctx}]
  (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
  (try (let [iviewr-q (-> (make-supply-question-msg ctx) ; This just makes a SUPPLY-QUESTION message-type.
                          (tell-interviewer ctx)         ; Returns (from the interviewer) a {:message-type "QUESTION-TO-ASK", :question "...'}
                          (separate-table)               ; Returns a {:text "..." :table-html "..." :table {:table-headings ... :tabel-data ...}}
                          (fix-off-course--question ctx))]         ; Currently a no-op. Returns argument.
         (when (= :human responder-type) (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))
         ;; This returns a VECTOR of statements from the interviewees and interviewer.
         (get-an-answer (cond-> ctx
                          true                                (assoc :question (:text iviewr-q))
                          (-> iviewr-q :table-html not-empty) (assoc :table-html (:table-html iviewr-q)))))
       (finally (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;TODO: add for other interviews
(defn analyze-response
  "Analyze the response from the expert (human or surrogate)."
  [pid cid response]
  (assert (s/valid? ::cid cid))
  (cond
    (= :process cid) (log! :info (str "process analysis " pid ": " response))
    (= :data cid) (dan/analyze-response pid response)
    (= :resources cid) (log! :info (str "resources analysis " pid ": " response))
    (= :optimality cid) (log! :info (str "optimality analysis" pid ": " response)))
  )

(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (case cid
    ;; ToDo: Someday we will be able to do better than this!
    (:resources :optimality) (find-claim '(flow-shop ?x) (db/get-claims pid))
    (:process :data) true))

(defn redirect-user-to-discussion
  "Put a message in the current chat to go to the recommended chat."
  [client-id current-chat recommended-chat]
  (let [text (str
              (format "We aren't quite ready to discuss %s until we've talked more about %s. "
                      (name current-chat) (name recommended-chat))
              (format "Could you pop over to the %s discussion (menu on left, 'Process', 'Data' etc.) and chat more with that agent?"
                      (name recommended-chat)))]
    ;; Note that you don't send cid; it lands in whatever conversation the users is looking at.
    (ws/send-to-chat {:dispatch-key :iviewr-says :client-id client-id :text text})))

(defn ctx-surrogate
  "Return context updated with surrogate info."
  [{:keys [pid cid force-new?] :as ctx}]
  (let [interviewer-agent (adb/ensure-agent! (-> (get @adb/agent-infos (-> cid name (str "-interview-agent") keyword))
                                                 (assoc :pid pid)))
        surrogate-agent   (adb/ensure-agent! (-> (get @adb/agent-infos pid)
                                                 (assoc :base-type pid)
                                                 (assoc :force-new? force-new?)))
        ctx (-> ctx
                (assoc :responder-type :surrogate)
                (assoc :interviewer-agent interviewer-agent)
                (assoc :surrogate-agent surrogate-agent))]
    (if (s/valid? ::surrogate-ctx ctx)
      ctx
      (throw (ex-info "Invalid surrogate context:" {:ctx ctx})))))

(defn ctx-human
  "Return  part of context specific to humans."
  [{:keys [pid cid]}]
   {:responder-type :human
    :interviewer-agent (adb/ensure-agent! (-> (get @adb/agent-infos (-> cid name (str "-interview-agent") keyword))
                                              (assoc :pid pid)))})

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
      pid))

(def iviewr-infos
  "These have all the info of agent-db/agent-infos entries and additional information for making instructions.
   To add these to agent-db/agent-infos, use force-new-interviewer!."
  (-> "agents/iviewrs/iviewr-infos.edn" io/resource slurp edn/read-string))

(defn write-agent-instructions!
  "Write the agent instructions to resources/agents/iviewrs/<cid>-iviewr-instructions.txt."
  [cid]
  (if-let [info (not-empty (get iviewr-infos cid))]
    (let [{:keys [iviewr-name focus]} info
          others (reduce (fn [r x] (if (= (:id x) cid) r (into r [(:iviewr-name x) (:focus x)]))) [] (vals iviewr-infos))
          instructions (str "You are one of four interviewers engaging humans in conversations to elicit from them requirements for a scheduling system we humans and AI agents will be creating together using MiniZinc.\n"
                            (format "You are the %s Agent; you ask questions about %s\n" iviewr-name focus)
                            "The other three agents are:"
                            (cl-format nil "~{~%     - a ~A Agent: that interviews about ~A~}" others)
                            "\nIn as far as it is practical, you should avoid asking questions in the areas that are the responsibility of these other agents.\n\n"
                            (-> "agents/iviewrs/base-iviewr-instructions.txt" io/resource slurp))]
      (spit (str "resources/agents/iviewrs/" (name cid) "-iviewr-instructions.txt") instructions))
    (log! :error (str "No conversation cid = " cid))))

;;; ToDo: Should we add a creation policy for interviewers? We need to force new ones whenever we are working on the system instructions.
(defn ^:diag force-new-interviewer!
  "As a matter of policy, shared-assistant type agents such as surrogates and interviewers don't update the assistant just because
   the instructions change. This is because we try to preserve the context already built up by the current agent.
   Therefore, if you don't care about that context but you do care about seeing the effect of new instructions, you should call this."
  ([cid] (force-new-interviewer! cid nil))
  ([cid pid]
   (assert (#{:process :data :resources :optimality} cid))
   (let [iviewr-info (not-empty (get iviewr-infos cid))
         {:keys [base-type] :as info} (cond-> (dissoc iviewr-info :iviewr-name :focus :id :warm-up-question)
                                        true (assoc :base-type (-> cid name (str "-interview-agent") keyword))
                                        true (assoc :force-new? true)
                                        pid  (assoc :pid pid))]
     (write-agent-instructions! cid)
     (swap! adb/agent-infos #(assoc % base-type (dissoc info :pid)))
     (adb/ensure-agent! info))))

(defn check-and-put-ds
  [iviewr-response {:keys [pid cid] :as _ctx}]
  (db/put-ds! pid cid iviewr-response))

;;; resume-conversation is typically called by client dispatch :resume-conversation.
;;; start-conversation can make it happen by asking the client to load-project (with cid = :process).
(defn resume-conversation
  "Resume the interview loop for an established project and given cid."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (s/valid? ::client-id client-id))
  (assert (s/valid? ::cid cid))
  (log! :debug (str "--------- Resume conversation: ctx: " ctx))
  (reset! course-correction-count 0)
  (try
    (if (not (ready-for-discussion? pid cid))
      (redirect-user-to-discussion client-id cid :process)
      ;; The conversation loop.
      (let [asking-role (-> (str cid "-interviewer") keyword)
            ctx (-> (if (surrogate? pid) (merge ctx (ctx-surrogate ctx)) (merge ctx (ctx-human ctx)))
                    (assoc :asking-role asking-role))]
        (when-not (db/conversation-done? pid cid)
          (if @active?
            (do (agent-log "resume " pid  " " cid)
                (-> (conversation-history pid cid) (tell-interviewer ctx))
                (loop [cnt 0]
                  (if @active?
                    ;; q-and-a does a SUPPLY-QUESTION and returns a vec of msg objects suitable for db/add-msg.
                    (let [conversation (q-and-a ctx)
                          expert-response (-> conversation last :full-text)] ; ToDo: This assumes the last is meaningful.
                      (log! :info (str "Expert in resume-conversation-loop: " expert-response))
                      (doseq [msg conversation]
                        (db/add-msg (merge {:pid pid :cid cid} msg)))
                      (db/put-budget! pid cid (- (db/get-budget pid cid) 0.05))
                      (analyze-response pid cid expert-response) ; <================================ NL add
                      (cond
                        (> cnt 10)                                  :exceeded-questions-safety-stop
                        ;; ToDo: Write some utility to "re-fund and re-open" conversations.
                        (<= (db/get-budget pid cid) 0)              (db/assert-conversation-done! pid cid)
                        :else
                        ;; Interviewer respond to INTERVIEWEES-RESPOND with either OK, PHASE-1-CONCLUSION, or DATA-STRUCTURE-REFINEMENT.
                        (let [iviewr-response (tell-interviewer {:message-type "INTERVIEWEES-RESPOND"
                                                                 :response expert-response}
                                                                ctx)]
                          (log! :info (str "Interviewer in resume-conversation-loop: " iviewr-response))
                          (when (surrogate? pid) (ru/refresh-client client-id pid cid))
                          (case (:message-type iviewr-response)
                            "PHASE-1-CONCLUSION"          (tell-interviewer (ru/eads-response! :process pid cid iviewr-response) ctx)
                            "DATA-STRUCTURE-REFINEMENT"   (check-and-put-ds iviewr-response ctx)
                            nil)
                          (recur (inc cnt)))))
                    (log! :warn "Exiting because active? atom is false."))))
            (log! :warn "Exiting because active? atom is false.")))))
    (finally (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;; The equivalent for surrogates is start-surrogate. It also asks the first question.
(defn start-conversation
  "Ask a human the first question (a warm-up question), create a project and call resume-conversation.
   :start-conversation is a dispatch-key from the client, providing, perhaps only the client-id."
  [{:keys [client-id cid] :as ctx}]
  (ws/send-to-chat {:dispatch-key :iviewr-says :client-id client-id :promise? true
                    :text (str "You want to start a new project? This is the right place! "
                               (db/conversation-intros :process))})
  (try
    (let [ctx (merge ctx {:responder-type :human
                          :human-starting? true
                          :question the-warm-up-type-question})
          conversation (get-an-answer ctx)
          response (-> conversation last :text) ; ToDo: Could be more to it...wants-a-break?.
          pid (start-human-project! response)
          [_ _ pname] (ru/find-claim '(project-name ?pid ?pname) (db/get-claims pid))]
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (doseq [{:keys [from text table tags]} conversation]
        (db/add-msg {:pid pid :cid :process :from from :tags tags :text text :table table}))
      (db/add-msg {:pid pid
                   :cid :process
                   :from :system
                   :text (str "Great, we'll call your project " pname ".")
                   :tags [:process/warm-up :name-project :informative]})
      ;; This will cause a resume-conversation:
      (ws/send-to-chat {:dispatch-key :load-proj :client-id client-id  :promise? false
                        :new-proj-map {:project/name pname :project/id pid}}))
    (finally
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :start-conversation start-conversation)
  (ws/register-ws-dispatch :resume-conversation resume-conversation))

(defstate iviewers
  :start (init-iviewers!))
