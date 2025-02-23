(ns scheduling-tbd.interviewing.interviewers
    "Runs an interview uing an interview agent."
    (:require
     [clojure.core.unify            :as uni]
     [clojure.data.xml              :as xml]
     [clojure.pprint                :refer [pprint]]
     [clojure.spec.alpha            :as s]
     [clojure.string                :as str]
     [jsonista.core                 :as json]
     [mount.core                    :as mount :refer [defstate]]
     [promesa.core                  :as p]
     [promesa.exec                  :as px]
     [scheduling-tbd.agent-db       :as adb]
     [scheduling-tbd.db             :as db]
     [scheduling-tbd.domain.data-analysis]
     [scheduling-tbd.domain.process-analysis  :as pan :refer [the-warm-up-type-question text-to-var]]
     [scheduling-tbd.domain.optimality-analysis]
     [scheduling-tbd.domain.resources-analysis]
     [scheduling-tbd.llm            :as llm]
     [scheduling-tbd.response-utils :as ru :refer [find-claim]]
     [scheduling-tbd.sutil          :as sutil :refer [elide output-struct2clj]]
     [scheduling-tbd.web.websockets :as ws]
     [taoensso.telemere             :as tel :refer [log!]]))

(def ^:diag diag (atom nil))
(s/def ::pid keyword?)
(s/def ::cid keyword?)

;;; From the REPL you can change the active? atom to false anytime you want things to stop
;;; (like when it is burning OpenAI asking the same question over and over ;^)).
;;; If it doesn't stop, it is probably the case that you need to test for it in more places.
(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))

;;; (chat-pair-aux {:pid :sur-fountain-pens :surrogate? true :agent-query "Describe your most significant scheduling problem in a few sentences."} {})
(defn chat-pair-aux
  "Run one query/response pair of chat elements with a human or a surrogate.
   Returns promise which will resolve to the original obj argument except:
     1) :agent-query is adapted from the input argument value for the agent type (human or surrogate)
     2) :response is added. Typically its value is a string."
  [{:keys [question table table-text surrogate-agent responder-type preprocess-fn tries client-id] :as ctx
    :or {tries 1 preprocess-fn identity}}]
  (log! :debug (str "ctx in chat-pair-aux: " (with-out-str (pprint ctx))))
  (let [prom (if (= :human responder-type)
               (do
                 (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id})
                 (ws/send-to-chat (-> ctx                        ; This cannot timeout.
                                      (assoc :text question)
                                      (assoc :table table)
                                      (assoc :promise? true)
                                      (assoc :dispatch-key :iviewr-says))))
               (px/submit! (fn [] ; surrogate responder...
                             (try
                               (adb/query-agent surrogate-agent  ; This can timeout.
                                                (str question "\n\n" table-text)
                                                {:tries tries
                                                 :preprocess-fn preprocess-fn})
                               (catch Exception e {:error e})))))]
    (-> prom p/await)))

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
(s/def ::tags (s/coll-of keyword?))
(s/def ::topic #(#{:process :data :resources :optimality} %))

;;; ToDo: Maybe return a string ===immoderate=== and end the conversation if needed.
(defn chat-pair
  "Call to run ask and wait for an answer, returns the response, a string.
   If response is immoderate return '===IMMODERATE==='."
  [{:keys [responder-type client-id] :as ctx}]
  (let [response-text (chat-pair-aux ctx)]
    (if (string? response-text)
      (if (and (= :human responder-type) (llm/immoderate? response-text))
        (do (ws/send-to-chat {:dispatch-key :iviewr-says :client-id client-id :text "This is not helpful."})
            "===IMMODERATE===") ; ToDo: Continue with this to stop interactions.
        response-text)
    (throw (ex-info "Unknown response from operator" {:response response-text})))))

;;; To check the structure of messages to and from the interviewer:
(s/def ::interviewer-msg (s/and (s/keys :req-un [::message-type])
                                #(let [{:keys [advice budget commit-notes conclusion convey-to-interviewees
                                               data-structure message-type question Q-A-pairs response status]} %]
                                   (case message-type
                                     "SUPPLY-QUESTION"           (s/valid? ::budget budget) ; _(s/valid? ::claims claims)
                                     "QUESTION-TO-ASK"           (s/valid? ::question question)
                                     "INTERVIEWEES-RESPOND"      (s/valid? ::response response)
                                     "DATA-STRUCTURE-REFINEMENT" (and (s/valid? ::commit-notes commit-notes)
                                                                      (s/valid? ::data-structure data-structure)
                                                                      (s/valid? ::convey-to-interviewees convey-to-interviewees))
                                     "PHASE-1-CONCLUSION"        (s/valid? ::phase-1-conclusion conclusion)
                                     #_#_"ANALYSIS-CONCLUDES"      (s/valid? ::conclusions conclusions)
                                     "COURSE-CORRECTION"       (s/valid? ::advice advice)
                                     "CONVERSATION-HISTORY"    (and (s/valid? ::budget budget)
                                                                    (s/valid? ::q-a-pairs Q-A-pairs))
                                     "STATUS"                  (s/valid? ::status status)
                                   nil))))

(s/def ::advice string?)
(s/def ::budget number?)
(s/def ::response string?)
(s/def ::status string?)
(s/def ::q-a-pairs (s/coll-of ::q-a-pair))
(s/def ::q-a-pair (s/keys :req-un [::question ::answer]))
(s/def ::question string?)
(s/def ::answer string?)

(def course-correction-count (atom 0))

(defn too-many-course-corrections!
  "If many course-corrections have occurred, stop the conversation."
  [{:keys [command]}]
  (when (= command "COURSE-CORRECTION")
    (swap! course-correction-count inc))
  (when (> @course-correction-count 5)
    (reset! active? false)))

(defn tell-interviewer
  "Send a command to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [msg {:keys [interviewer-agent cid] :as _ctx}]
  (too-many-course-corrections! msg)
  (when-not (s/valid? ::interviewer-msg msg) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid interviewer-msg: " (with-out-str (pprint msg)))))
  (log! :info (-> (str "Interviewer told: " msg) (elide 150)))
  (let [msg-string (json/write-value-as-string msg)
        res (-> (adb/query-agent interviewer-agent msg-string) output-struct2clj)]
    (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
    res))

(defn response-analysis
  "Return a map of booleans including
    - :answers-the-question? : which is true (some text) if the answer to the Q/A pair appears to answer the question.
    - :raises-a-question? : which is true (some text) if the response raises a question.
    - :wants-a-break?  : which is true (some text) if the user explicitly asks for a break?"
  [q-txt a-txt]
  (assert (string? q-txt))
  (assert (string? a-txt))
  (-> (adb/query-agent :response-analysis-agent (format "QUESTION: %s \nRESPONSE: %s" q-txt a-txt))
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
(defn get-an-answer
  "Get a response to the argument question, looping through non-responsive side conversation, if necessary.
   Return a vector of the conversation (objects suitable for db/add-msg) terminating in an answer to the question.
   Might talk to the client to keep things moving if necessary. Importantly, this requires neither PID nor CID."
  [{:keys [question responder-type human-starting?] :as ctx}]
  (when-not human-starting? (s/assert ::chat-pair-ctx ctx))
  (let [conversation [{:text question :from :system :tags [:query]}]
        response  (chat-pair ctx) ; response here is text or a table (user either talking though chat or hits 'submit' on a table.
        answered? (= :surrogate responder-type) ; Surrogate doesn't beat around the bush, I think!
        {:keys [answers-the-question? raises-a-question? wants-a-break?]} (when-not answered? (response-analysis question response))
        answered? (or answered? answers-the-question?)]
    (if (not (or answered? wants-a-break? raises-a-question?))
      (let [same-question (str "Okay, but we are still interested in this: " question)]
        (into conversation (get-an-answer (-> ctx (assoc :question same-question)))))
      (cond-> conversation
        answered?                  (conj {:text response :from responder-type :tags [:response]})
        wants-a-break?             (into (handle-wants-a-break question response ctx))
        raises-a-question?         (into (handle-raises-a-question question response ctx))
        (not
         (or answered?
             wants-a-break?
             raises-a-question?))  (into (handle-other-non-responsive question response ctx))))))

;;; ToDo: It isn't obvious that the interview agent needs to see all this. Only useful when the thread was destroyed?
;;;       It might be sufficient to explain the claims. ToDo: V4 doesn't have claims, but this still holds
(defn conversation-history
  "Construct a conversation-history command structure for the argument project.
   This tells the interview agent what has already been asked. If the thread hasn't been deleted
   (i.e. by OpenAI, because it is more than 30 days old) then the agent knows what has already been
   asked EXCEPT for the warm-up question (if any) which happens before the interview."
  [pid cid]
  (let [msgs (db/get-conversation pid cid)
        questions (filter #(= :system (:message/from %)) msgs)] ; Some aren't questions; we deal with it.
    {:message-type "CONVERSATION-HISTORY"
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
                     vec)}))

(defn make-supply-question-msg
  "Create a supply question command for the conversation."
  [{:keys [pid cid] :as _ctx}]
  {:message-type "SUPPLY-QUESTION" :budget (db/get-budget pid cid)})

(defn fix-off-course [q ctx] q) ; ToDo: NYI

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
                      (mapv #(-> {} (assoc :title %) (assoc :key (-> % ru/text-to-var keyword)))))
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
   Return an object where :question is the argument string minus the table, and :table-text is the substring between the markers."
  [text]
  (let [in-table? (atom false)]
    (loop [lines (str/split-lines text)
           res {:question "" :table-text ""}]
      (let [l (first lines)]
        (when (and l @in-table? (re-matches #"(?i)^\s*\#\+end_src\s*" l))  (reset! in-table? false))
        (when (and l (re-matches #"(?i)^\s*\#\+begin_src\s+HTML\s*" l))    (reset! in-table? true))
        (if (empty? lines)
          res
          (recur (rest lines)
                 (cond (re-matches #"(?i)^\s*\#\+begin_src\s+HTML\s*" l) res
                       (re-matches #"(?i)^\s*\#\+end_src\s*"          l) res
                       (not @in-table?) (update res :question   #(str % "\n" l))
                       @in-table?       (update res :table-text #(str % "\n" l)))))))))

(defn separate-table
  "Return the response with XHTML tables separated from text and converted to a clojure object."
  [{:keys [question]}]
  (let [{:keys [table-text] :as res} (separate-table-aux question)]
        (if (not-empty table-text)
          (try (as-> res ?r
                 (assoc ?r :table (-> ?r :table-text java.io.StringReader. xml/parse table-xml2clj table2obj))
                 (assoc ?r :status :ok))
               ;; ToDo:  This catch is not catching!?!
               (catch Throwable _e (assoc res :status :invalid-table)))
          res)))

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call q-and-a
;;; over and over and the interview will advance each time.
(defn q-and-a
  "Call interviewer to supply a question; call get-an-answer for an answer prefaced by zero or more messages non-responsive to the question.
   Returns a vector of message objects suitable for db/add-msg."
  [{:keys [client-id] :as ctx}]
  (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
  (try (let [interviewer-q (-> (make-supply-question-msg ctx) ; This just makes the question map.
                               (tell-interviewer ctx)         ; Returns (from the interviewer) a {:message-type "QUESTION-TO-ASK", :question "...'}
                               (separate-table)               ; Returns a {:question "..." :table-text "..." :table {:table-headings ... :tabel-data ...}}
                               (fix-off-course ctx))]  ; Currently a no-op. Returns argument.
         (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id})
         (reset! diag interviewer-q)
         (get-an-answer (merge ctx interviewer-q)))
       (finally (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (case cid
    ;; ToDo: Someday we will be able to do better than this!
    (:data :resources :optimality) (find-claim '(flow-shop ?x) (db/get-claims pid))
    :process true))

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

(defn surrogate? [pid] (ru/find-claim '(surrogate ?x) (db/get-claims pid)))

(defn ctx-surrogate
  "Return context updated with surrogate info."
  [{:keys [pid cid] :as ctx}]
  (let [interviewer-agent (adb/ensure-agent! (-> (get @adb/agent-infos (-> cid name (str "-interview-agent") keyword))
                                                 (assoc :pid pid)))
        surrogate-agent   (adb/ensure-agent! (-> (get @adb/agent-infos pid)
                                                 (assoc :base-type pid)))
        ctx (-> ctx
                (dissoc :client-id :dispatch-key)
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
  (let [warm-up-claims (pan/analyze-warm-up-response response)
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

;;; resume-conversation is typically called by client dispatch :resume-conversation.
;;; start-conversation can make it happen by asking the client to load-project (with cid = :process).
(defn resume-conversation
  "Resume the interview loop for an established project and given cid."
  [{:keys [client-id pid cid] :as ctx}]
  (assert (string? client-id))
  (assert (#{:process :data :resources :optimality} cid))
  (log! :debug (str "--------- Resume conversation: ctx: " ctx))
  (reset! course-correction-count 0)
  (try
    (if (and (not= :process cid) (not (ready-for-discussion? pid cid)))
      (redirect-user-to-discussion client-id cid :process)
      ;; The conversation loop.
      (let [ctx (if (surrogate? pid) (merge ctx (ctx-surrogate ctx)) (merge ctx (ctx-human ctx)))]
        (when-not (db/conversation-done? pid cid)
          (if @active?
            (do (-> (conversation-history pid cid) (tell-interviewer ctx))
                (loop [cnt 0
                       conversation (q-and-a ctx)] ; Returns a vec of msg objects suitable for db/add-msg.
                  (if @active?
                    (let [response (-> conversation last)]
                      (doseq [msg conversation]
                        (db/add-msg (merge {:pid pid :cid cid} msg)))
                      (db/put-budget! pid cid (- (db/get-budget pid cid) 0.05))
                      (cond
                        (> cnt 20)                                  :exceeded-questions-safety-stop
                        ;; ToDo: Write some utility to "re-fund and re-open" conversations.
                        (<= 0 (db/get-budget pid cid)) (db/assert-conversation-done! pid cid)
                        :else
                        (do
                          (tell-interviewer {:message-type "INTERVIEWEES-RESPOND"
                                             :response (:text response)}
                                            ctx)
                          (when (surrogate? pid) (ru/refresh-client client-id pid cid))
                          (recur (inc cnt)
                                 (q-and-a ctx)))))
                    (log! :warn "Exiting because active? atom is false."))))
            (log! :warn "Exiting because active? atom is false.")))))
    (finally (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))

(defn start-conversation
  "Ask a human the first question, create a project and call resume-conversation.
   :start-conversation is a dispatch-key from the client, providing, perhaps only the client-id."
  [{:keys [client-id] :as ctx}]
  (ws/send-to-chat {:dispatch-key :iviewr-says :client-id client-id :promise? true
                    :text (str "You want to start a new project? This is the right place! "
                               (db/conversation-intros :process))})
  (try
    (let [ctx (merge ctx {:responder-type :human
                          :human-starting? true
                          :question the-warm-up-type-question})
          conversation (get-an-answer ctx)
          response (-> conversation last :text) ; ToDo: Could be more to it...wants-a-break?.
          _effect! (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
          pid (start-human-project! response)
          [_ _ pname] (ru/find-claim '(project-name ?pid ?pname) (db/get-claims pid))]
      (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (doseq [m conversation]
        (let [{:keys [from text tags]} m]
          (db/add-msg {:pid pid :cid :process :from from :tags tags :text text})))
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
