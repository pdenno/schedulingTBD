(ns scheduling-tbd.iviewr.interviewers
  "Runs an interview using an interview agent."
  (:require
   [cheshire.core :as ches]
   [clojure.data.xml :as xml]
   [clojure.pprint :refer [pprint cl-format]]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [mount.core :as mount :refer [defstate]]
   [promesa.core :as p]
   [promesa.exec :as px]
   [scheduling-tbd.agent-db :as adb :refer [agent-log]]
   [scheduling-tbd.db :as db]
   [scheduling-tbd.llm :as llm]
   [scheduling-tbd.iviewr.eads] ; for mount
   [scheduling-tbd.iviewr.eads-util :as eu :refer [combine-ds!]]
   [scheduling-tbd.iviewr.ork :as ork]
   [scheduling-tbd.iviewr.response-utils :as ru]
   [scheduling-tbd.minizinc :as mzn]
   [scheduling-tbd.mock  :as mock :refer [continue-mocking?]]
   [scheduling-tbd.specs :as specs]
   [scheduling-tbd.sutil :as sutil :refer [elide mocking? ai-response2clj shadow-pid log!]]
   [scheduling-tbd.util :as util :refer [now]]
   [scheduling-tbd.web.websockets :as ws]
   [scheduling-tbd.ds2mermaid :as ds2m]))

;;; The usual way of interviews involves q-and-a called from resume-conversation.
;;; [resume-conversation] -> q-and-a -> get-an-answer -> chat-pair -> send-to-client or query-agent (sur).

(def ^:diag active? "Debugging tool to stop the interview when false." (atom true))
(def ^:diag diag (atom nil))
(s/def ::cid #(#{:process :data :resources :optimality} %))

;;; From the REPL you can change the active? atom to false anytime you want things to stop
;;; (like when it is burning OpenAI asking the same question over and over ;^)).
;;; If it doesn't stop, it is probably the case that you need to test for it in more places.

;;; For use of chat-pair and other things defined below.
(s/def ::chat-pair-ctx (s/and ::common-ctx
                              (s/or :surrogate ::surrogate-ctx :human ::human-ctx)))

(s/def ::common-ctx (s/keys :req-un [::client-id ::question ::cid]))
(s/def ::surrogate-ctx (s/keys :req-un [:sur/responder-type ::surrogate-agent ::iviewr-agent]))
(s/def ::human-ctx (s/keys :req-un [:hum/responder-type ::iviewr-agent]))
(s/def ::basic-agent (s/keys :req-un [::aid ::tid ::base-type]))
(s/def ::iviewr-agent ::basic-agent)
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
    :or {tries 1 preprocess-fn identity}}]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (log! :debug (str "ctx in chat-pair-aux:\n" (with-out-str (pprint ctx))))
    (let [prom (if (= :human responder-type)
                 (ws/send-to-client (-> ctx ; This cannot timeout.
                                        (assoc :full-text question)
                                        (assoc :table table)
                                        (assoc :promise? true)
                                        (assoc :dispatch-key :iviewr-says)))
                 (px/submit! (fn [] ; surrogate responder...
                               (try
                                 (adb/agent-log (cl-format nil "{:log-comment \"[~A interviewer] (asking interviewees):~%~A~%Table:~%~A \"}"
                                                         (name cid) question table-text)
                                                {:console? true})
                                 (adb/query-agent surrogate-agent ; This can timeout.
                                                  (str question "\n\n" table-text)
                                                  {:tries tries
                                                   :base-type pid
                                                   :preprocess-fn preprocess-fn})
                                 (catch Exception e {:error e})))))]
      (-> prom p/await))))

(declare separate-table)

(defn chat-pair
  "Call to run ask and wait for an answer.
   It returns a map {:msg-type :expert-response :full-text <string> :text <string> :table <table-map>} where
        :msg-type is either :expert-response or :immoderate.
        :full-text is text in response to the query (:question ctx), especially useful for use with surrogates.
        :text is text in response to the query with surrogate tables removed.
        :table is a table map completed by the expert.
   If response is immoderate returns {:msg-type :immoderate}"
  [{:keys [responder-type cid] :as ctx}]
  (let [response (chat-pair-aux ctx)]
    (agent-log (cl-format nil "{:log-comment \"[~A interviewer] (response from interviewees):~%~A\"}" (name cid) response)
               {:console? true :elide-console 130})
    (case responder-type
      :human (if (-> response :full-text llm/immoderate?)
               {:msg-type :immoderate}
               response)
      ;; ToDo: Move the let up so can capture tables correctly with human too.
      :surrogate (let [{:keys [full-text text table-html table]} (separate-table response)]
                   (cond-> {:msg-type :expert-response :full-text full-text}
                     text (assoc :text text)
                     table (assoc :table table)
                     table (assoc :table-html table-html))))))

(defn tell-interviewer
  "Send a message to an interviewer agent and wait for response; translate it.
   :aid and :tid in the opts should be for the interviewer agent."
  ([msg iviewr-agent cid] (tell-interviewer msg iviewr-agent cid {}))
  ([msg iviewr-agent cid opts]
   (when @active?
     (when-not (s/valid? ::specs/interviewer-msg msg) ; We don't s/assert here because old project might not be up-to-date.
       (log! :warn (str ";;; Invalid interviewer-msg:\n" (with-out-str (pprint msg)))))
     (agent-log (cl-format nil "{:log-comment \"[interview manager] (tells ~A interviewer):~%~A\"}"
                           (name cid) (with-out-str (pprint msg))))
     (log! :info (-> (str "Interviewer told: " msg) (elide 150)))
     (let [msg-string (ches/generate-string msg {:pretty true})
           res (as-> (adb/query-agent iviewr-agent msg-string opts) ?r
                 (when ?r (ai-response2clj ?r))
                 (when ?r  (cond-> ?r
                             (contains? ?r :message-type) (update :message-type keyword))))]
       (log! :info (-> (str "Interviewer returns: " res) (elide 150)))
       (agent-log (cl-format nil "{:log-comment \"[interview manager] (receives from ~A interviewer)~%~A\"}"
                             (name cid) (with-out-str (pprint res))))
       (if (or (nil? res) (and @mocking? (not (mock/continue-mocking?))))
         {:message-type :mocking-complete}
         (reset! diag res))))))

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
    (agent-log (cl-format nil "{:log-comment \"[response analysis agent] (concludes):~%~A\"}" (with-out-str (pprint res))))
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
  (assert (string? question))
  (when-not human-starting?
    (when-not (s/valid? ::chat-pair-ctx ctx)
      (throw (ex-info "Invalid context in get-an-answer." {:ctx ctx}))))
  (let [conversation [{:full-text question :from :system :tags [:query]}]
        {:keys [full-text] :as response} (chat-pair ctx) ; response here is text or a table (user either talking though chat or hits 'submit' on a table.
        answered? (= :surrogate responder-type) ; Surrogate doesn't beat around the bush, I think!
        {:keys [answers-the-question? raises-a-question? wants-a-break?]} (when (and (not answered?) (string? full-text))
                                                                            (response-analysis question full-text ctx))
        answered? (or answered? answers-the-question?)]
    (if (not (or answered? wants-a-break? raises-a-question?))
      (let [same-question (ask-again question)]
        (into conversation (get-an-answer (-> ctx (assoc :question same-question)))))
      (cond-> conversation
        answered? (conj (-> response
                            (assoc :from responder-type)
                            (assoc :tags [:response])))
        wants-a-break? (into (handle-wants-a-break question response ctx))
        raises-a-question? (into (handle-raises-a-question question response ctx))
        (not
         (or answered?
             wants-a-break?
             raises-a-question?)) (into (handle-other-non-responsive question response ctx))))))

(defn surrogate? [pid]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (ru/find-claim '(surrogate ?x) (db/get-claims pid))))

(defn make-supply-question-msg
  "Create a supply question message for the conversation."
  [pid]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    {:message-type :SUPPLY-QUESTION :budget (db/get-questioning-budget-left! pid (db/get-project-active-EADS-id pid))}))

;;; --------------- Handle tables from interviewer --------------------------------------
(defn table-xml2clj
  "Remove some useless aspects of the parsed XHTML, including string content and attrs."
  [table-xml]
  (letfn [(x2c [x]
            (cond (seq? x) (mapv x2c x)
                  (map? x) (reduce-kv (fn [m k v]
                                        (cond (= k :content) (assoc m k (->> v (remove #(and (string? %) (re-matches #"^\s+" %))) vec x2c))
                                              (= k :attrs) m ; There aren't any values here in our application.
                                              :else (assoc m k (x2c v))))
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
                      (mapv #(-> {}
                                 (assoc :title %)
                                 (assoc :key (-> % (ru/text-to-var) keyword)))))
                 (log! :warn "No titles in table2obj."))
        title-keys (map :key titles)
        data-rows (->> ugly-table
                       :content ; table content
                       rest ; everything but header
                       (mapv (fn [row]
                               (mapv #(or (-> % :content first) "") (:content row)))))]
    {:table-headings titles
     :table-body (mapv (fn [row] (zipmap title-keys row)) data-rows)}))

(def table-error-text (atom nil))

(defn separate-table-aux
  "The argument is a text string which may contain a single block of HTML (a table, it turns out) between the markers '#+begin_src HTML ... #+end_src'.
   (In this function we don't care what is between the markers, but in usage it is typically a single HTML table.)
   Return a map where
      :full-text is the argument text,
      :text is the argument string minus the content between the marker, and
      :table-html is the substring between the markers. If no markers were found an empty string will be returned."
  [text]
  (assert (string? text))
  (let [in-table? (atom false)
        diag-found-table? (atom false)
        first-table-line? (atom true)
        result (loop [lines (str/split-lines text)
                      res {:full-text text :text "" :table-html ""}]
                 (let [l (first lines)]
                   (if (empty? lines)
                     res
                     (recur (rest lines)
                            (cond
                              ;; Table begin marker - skip this line and enter table mode
                              (and l (not @in-table?) (re-matches #"(?i)^\s*\#\+begin_src\s+HTML\s*" l))
                              (do (reset! diag-found-table? true)
                                  (reset! in-table? true)
                                  (reset! first-table-line? true)
                                  res)

                              ;; End marker for table - skip this line, exit table mode
                              (and l @in-table? (re-matches #"(?i)^\s*\#\+end_src\s*" l))
                              (do (reset! in-table? false)
                                  ;; Add trailing newline to table-html
                                  (-> res
                                      (update :table-html #(str % "\n"))
                                      (update :text #(if (empty? %) "" (str % "\n")))))

                              ;; Regular text outside table
                              (and l (not @in-table?))
                              (update res :text #(if (empty? %) l (str % "\n" l)))

                              ;; Content inside table - add leading newline for first line
                              (and l @in-table?)
                              (if @first-table-line?
                                (do (reset! first-table-line? false)
                                    (update res :table-html #(str % "\n" l)))
                                (update res :table-html #(str % "\n" l)))

                              ;; Empty line case
                              :else res)))))]
    ;; Check if we found table markers but no actual content (just whitespace/newlines)
    (when (and @diag-found-table? (-> result :table-html str/trim empty?))
      (reset! table-error-text text)
      (throw (ex-info "Table markers; no table." {:text text})))
    result))

(defn separate-table
  "Because this is called both on questions and answers, arg may be
   (1) a string that might contain an embedded table, or
   (2) a context map that contains :question
   Because a surrogate can embed a table we have to call separate-table from chat-pair.
   This is the (2) usage and the users gets special presentation of a table because of it.
   Return a map containing :full-text (a string) :text (a string) :table-html (a string)  and :table (a map).
   Where :text is a substring of :full-text (argument text)."
  [arg]
  (assert (or (string? arg)
              (and (map? arg) (contains? arg :question))))
  (let [text (if (string? arg) arg (:question arg))
        {:keys [table-html] :as res} (separate-table-aux text)]
    (if (not-empty table-html)
      (try
        ;; Clean up HTML entities for XML parsing
        (let [clean-html (-> table-html
                             (str/replace "&" "&amp;") ; Fix unescaped ampersands
                             (str/replace "&amp;amp;" "&amp;") ; Avoid double-escaping
                             (str/replace "&amp;lt;" "&lt;")
                             (str/replace "&amp;gt;" "&gt;"))
              parsed-table (-> clean-html java.io.StringReader. xml/parse table-xml2clj table2obj)]
          (-> res
              (assoc :table parsed-table)
              (assoc :status :ok)))
        (catch Throwable _e (assoc res :status :invalid-table)))
      res)))

(s/def ::question string?)

(defn mocking-complete-msg?
  [msg]
  (or (not (mock/continue-mocking?))
      (and (map? msg) (= :mocking-complete (:message-type msg)))))

;;; Hint for diagnosing problems: Once you have the ctx (stuff it in an atom) you can call q-and-a
;;; over and over and the interview will advance each time.
(defn q-and-a
  "Call interviewer to supply a question; call get-an-answer for an answer prefaced by zero or more messages non-responsive to the question.
   Returns a vector of message objects suitable for db/add-msg (contains :pid :cid :from, :full-text :table...).
   The first element in the vector should be the question object, and the last the answer object."
  [iviewr-agent pid cid {:keys [client-id responder-type] :as ctx}]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (when @active?
      (ws/send-to-client {:dispatch-key :interviewer-busy? :value true :client-id client-id})
      (try (let [iviewr-q (as-> (make-supply-question-msg pid) ?m ; This just makes a SUPPLY-QUESTION message-type.
                            (tell-interviewer ?m iviewr-agent cid) ; Returns (from the interviewer) a {:message-type :QUESTION-TO-ASK, :question "...'}
                            (if (mocking-complete-msg? ?m) ?m (separate-table ?m)))] ; Returns a {:full-text "..." :table-html "..." :table {:table-headings ... :tabel-data ...}}
             (when (= :human responder-type) (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id}))
             ;; This returns a VECTOR of statements from the interviewees and interviewer.
             (when-not (mocking-complete-msg? iviewr-q)
               (get-an-answer (cond-> ctx
                                true (assoc :question (:full-text iviewr-q))
                                (-> iviewr-q :table-html not-empty) (assoc :table-html (:table-html iviewr-q))))))
           (finally (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id}))))))

;;; ToDo: This needs work. You should be able to reopen a conversation at any time.
(defn ready-for-discussion?
  "Return true if the state of conversation is such that we can now have
   discussion on the given topic."
  [pid cid]
  (let [pid (if @mocking? (shadow-pid pid) pid)
        cid (or cid (db/get-active-cid pid))] ; See chat.cljs for why cid might be nil.
    (letfn [(done? [status] (= status :eads-exhausted))]
      (let [[pstat dstat rstat _ostat] (doall (for [c [:process :data :resources :optimality]]
                                                (db/get-conversation-status pid c)))]
        (case cid
          :resources (and (done? pstat) (done? dstat))
          :optimality (and (done? pstat) (done? dstat) (done? rstat))
          :data (not (done? dstat))
          :process (not (done? pstat)))))))

(declare resume-conversation tell-interviewer ctx-surrogate)

;;; ToDo: This needs work. You should be able to reopen a conversation at any time. Maybe need an agent for this!
(defn find-appropriate-conv-and-redirect
  "If we think the conversation is over, ask them if they want to reopen it.
   Put a message in the current chat to go to the recommended chat or
   Typically this is called with ready-for-discussion? returned false."
  [{:keys [pid cid client-id] :as ctx}]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (letfn [(ready? [status] (#{:not-started :in-progress} status))]
      (let [[pstat dstat rstat ostat] (doall (for [c [:process :data :resources :optimality]]
                                               (db/get-conversation-status pid c)))
            better-choice (case cid
                            :process (cond (ready? dstat) :data
                                           (ready? rstat) :resources
                                           (ready? ostat) :optimality
                                           :else :all-done!)
                            :data (cond (ready? pstat) :process
                                        (ready? rstat) :resources
                                        (ready? ostat) :optimality
                                        :else :all-done!)
                            :resources (cond (ready? pstat) :process
                                             (ready? dstat) :data
                                             (ready? ostat) :optimality
                                           :else :all-done!))
            text (if (= :all-done! better-choice)
                   "We are satisfied with this conversation as it is. Did you want to reopen it for discussion?"
                   (str "At this point in our work, it would be better to discuss " (name better-choice) ". "
                        "Could you select " (-> better-choice name str/capitalize) " from the menu on the left?"))]
        (if (= client-id :console)
          (let [{:keys [iviewr-agent]} (merge ctx (ctx-surrogate ctx))]
            (db/put-active-cid! pid better-choice) ; ToDo: This hasn't been tested. Is that enough?
            (when-let [eads-id (db/get-active-EADS-id pid better-choice)]
              (tell-interviewer (db/get-EADS-instructions eads-id) iviewr-agent better-choice))
            (resume-conversation (assoc ctx :cid better-choice)))
          (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :text text}))))))

(defn ctx-surrogate
  "Return context updated with surrogate info.
   No need for mocking provisions here; adb/ensure-agent takes care of it."
  [{:keys [pid cid force-new?] :as ctx}]
  (let [;pid (if @mocking? (shadow-pid pid) pid)
        cid (or cid (db/get-active-cid pid)) ; See chat.cljs, which suggests there might be reasons it will send cid=nil.
        iviewr-agent (adb/ensure-agent! {:base-type (-> cid name (str "-interview-agent") keyword) :pid pid})
        surrogate-agent (adb/ensure-agent! {:base-type pid :pid pid} {:force-new? force-new?})
        ctx (-> ctx
                (assoc :cid cid)
                (assoc :responder-type :surrogate)
                (assoc :iviewr-agent iviewr-agent)
                (assoc :surrogate-agent surrogate-agent))]
    (if (s/valid? ::surrogate-ctx ctx)
      ctx
      (do (reset! diag ctx)
          (throw (ex-info "Invalid surrogate context:" {:ctx ctx}))))))

(defn ctx-human
  "Return  part of context specific to humans."
  [{:keys [pid cid]}]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    {:responder-type :human
     :iviewr-agent (adb/ensure-agent! {:base-type (-> cid name (str "-interview-agent") keyword) :pid pid})}))

(defn resume-exit
  "Clean things up, or at least produce a log message."
  [state-change ctx]
  (let [status (->> {:active?-atm @active?}
                    (merge ctx)
                    (merge state-change))]
    (agent-log (cl-format nil "{:log-comment \"resume-conversation exiting. status :~%~A\"}"
                          (with-out-str (pprint status))
                          {:console? true}))))

(defn resume-update-db-cid
  "Update DB for a new conversation."
  [pid cid old-cid]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (db/put-active-cid! pid cid)
    (db/put-conversation-status! pid old-cid :eads-exhausted)
    (db/put-conversation-status! pid cid :in-progress)))

(defn resume-update-db-eads
  "Update DB for new EADS instructions."
  [pid cid new-eads-id]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (agent-log (cl-format nil "{:log-comment \" Changing EADS instructions to ~A.\"}" new-eads-id)
               {:console? true :level :warn})
    (db/put-new-summary-ds! pid new-eads-id)
    (db/put-project-active-EADS-id! pid new-eads-id) ; This will be used by :message/pursuing-EADS.
    (db/put-active-EADS-id pid cid new-eads-id))) ; This will be used by :message/pursuing-EADS.

(defn put-DSR-on-msg!
  "Clean-up the DSR and save it on a property :message/EADS-data-structure
   of the interviewee response which is responsible for this update (the current response
   in the argument conversation)."
  [pid cid iviewr-response]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (letfn [(up-keys [obj]
              (cond (map? obj) (reduce-kv (fn [m k v] (assoc m (keyword k) (up-keys v))) {} obj)
                    (vector? obj) (mapv up-keys obj)
                    :else obj))]
      (let [obj (-> iviewr-response
                    up-keys
                    (update :message-type keyword)
                    (update :EADS-ref keyword))]
        (db/put-EADS-ds! pid cid obj)))))

(defn update-db-conversation!
  "Write to the project DB messages that occurred inclusive of a q/a pair.
   If there is a EADS being pursued, set the pursuing-EADS.
   Mark the last message as answering the first.
   Argument msgs-vec is a vector of maps having (possibly) :from :text :table :tags, and :question-type."
  [pid cid msgs-vec]
  (let [pid (if @mocking? (shadow-pid pid) pid)
        msg-ids (atom [])
        EADS-id (db/get-active-EADS-id pid cid)]
    (doseq [msg msgs-vec]
      (swap! msg-ids conj (db/add-msg (cond-> (merge {:pid pid :cid cid} msg)
                                        EADS-id (assoc :pursuing-EADS EADS-id)))))
    (let [first-msg-id (first @msg-ids)
          last-msg-id (last @msg-ids)]
      (when-not (= first-msg-id last-msg-id)
        (db/update-msg pid cid last-msg-id {:message/answers-question first-msg-id})))))

(defn resume-post-ui-actions
  "Send client actions to perform such as loading graphs or (for humans) changing conversations."
  [iviewr-response pid cid {:keys [new-cid? client-id responder-type] :as _ctx}]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (when-not (= client-id :console)
      (when (surrogate? pid) (ru/refresh-client client-id pid cid))
      (when (= :DATA-STRUCTURE-REFINEMENT (:message-type iviewr-response))
        ;; Using the data structure being pursued in some EADS instructions to show a FFBD of their processes.
      (when-let [eads-id (db/get-active-EADS-id pid cid)]
        (let [{:keys [EADS-ref] :as ds} (db/get-summary-ds pid eads-id)]
          (when (#{:process/flow-shop :process/job-shop--classifiable} EADS-ref)
            (let [graph-mermaid (ds2m/ds2mermaid ds)]
              (ws/send-to-client {:client-id client-id :dispatch-key :load-graph :graph graph-mermaid})))))))
    ;; Of course, we don't switch the conversation for humans; we have to tell them to switch.
    (when (and new-cid? (= :human responder-type))
      (ws/send-to-client {:dispatch-key :iviewr-says :client-id client-id :promise? true
                          :text (str "Right now we don't have much more to ask on this conversation, "
                                     "but we'd like to continue on the " (-> cid name str/capitalize) "conversation. "
                                     "Would you mind switching over there? (Use menu on the right.)")}))))

(defn resume-post-db-actions!
  "If the interviewer responded with a DATA-STRUCTURE-REFINEMENT (which is typical), associate the
   whole message as a string to the message which is an answer to the question."
  [iviewr-response pid cid eads-id]
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (db/reduce-questioning-budget! pid eads-id)
    (try (when (= :DATA-STRUCTURE-REFINEMENT (:message-type iviewr-response))
           (put-DSR-on-msg! pid cid iviewr-response)
           (when-let [eads-id (db/get-active-EADS-id pid cid)]
             (combine-ds! eads-id pid))
           (when (and (#{:process/flow-shop} eads-id)
                      (eu/ds-complete? eads-id pid))
             (when-let [minizinc (mzn/get-best-minizinc (db/get-summary-ds pid eads-id))]
               (let [mid (db/max-msg-id pid cid)]
                 (db/update-msg pid cid mid {:message/code minizinc})))))
         (catch Exception _e
           (log! :error "Error in post-db-action!. Continuing.")
           (reset! diag {:iviewr-response iviewr-response :pid pid :cid cid})))))

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
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (ork/ensure-ork! pid)
    (if (or (not @active?) (= :paused (db/get-execution-status pid)))
      {:forced-stop true :cid cid}
      ;; Otherwise do an analysis...
      (let [budget-ok? (> (db/get-questioning-budget-left! pid (db/get-project-active-EADS-id pid)) 0)
            active-eads-id (db/get-project-active-EADS-id pid)
            active-eads-complete? (or (not budget-ok?)
                                      (and active-eads-id (eu/ds-complete? active-eads-id pid)))
            new-eads-id (when (or active-eads-complete? (not active-eads-id))
                          (ork/get-new-EADS-id pid))
            change-eads? new-eads-id
            eads-id (or new-eads-id active-eads-id)
            exhausted? (not eads-id)
            new-cid (when new-eads-id
                      (let [nspace (when eads-id (-> eads-id namespace keyword))]
                        (when (not= cid nspace) nspace)))]
        (when-not budget-ok?
          (agent-log (cl-format nil "{:log-comment \"[ork manager] (in ork-review) switching to new EADS instructions ~A owing to budget.\"}" new-eads-id)
                     {:console? true :level :warn}))
        (when new-cid
          (agent-log (cl-format nil "{:log-comment \"[ork manager] (in ork-review) changing conversation to ~A.\"}" new-cid)))
        (let [res (cond-> {}
                    exhausted? (assoc :exhausted? true)
                    change-eads? (assoc :change-eads? true)
                    new-eads-id (assoc :new-eads-id new-eads-id)
                    new-cid (assoc :new-cid? true :old-cid cid :cid new-cid)
                    (not new-cid) (assoc :cid cid))]
          (agent-log (cl-format nil "{:log-comment \"[ork manager] (in ork-review) returning decision:~%~A\"}"
                                (with-out-str (pprint res)))
                     {:console? true})
          res)))))

;;; (inv/ork-force-new-thread :sur-craft-beer)
(defn ^:diag ork-force-new-thread
  [pid]
  (let [pid (if @mocking? (shadow-pid pid) pid)
        ork (db/get-agent {:base-type :orchestrator-agent :pid pid})
        tid (adb/get-llm-thread-id ork)
        ork-with-thread (assoc ork :agent/thread-id tid)]
    (db/put-agent! {:base-type :orchestrator-agent :pid pid} ork-with-thread)))

;;; resume-conversation is typically called by client dispatch :resume-conversation.
;;; (inv/resume-conversation {:client-id :console :pid :sur-craft-beer :cid :process})
(defn resume-conversation
  "Resume the interview loop for an established project and given cid."
  [{:keys [client-id pid cid] :as ctx}] ; cid might be nil here.
  (let [pid (if @mocking? (shadow-pid pid) pid)]
    (assert (s/valid? ::client-id client-id))
    (agent-log (cl-format nil "{:log-comment \"============= resume-conversation: ~A ~A ~A ========================\"}"
                          pid cid (str (now))) {:console? true :level :debug})
    (if (ready-for-discussion? pid cid)
      (try
        (let [{:keys [iviewr-agent cid] :as ctx} (if (surrogate? pid) (merge ctx (ctx-surrogate ctx)) (merge ctx (ctx-human ctx)))
              history (ork/conversation-history pid)]
          ;; Things to do when r-c is called for actual resuming, instead of starting fresh.
          ;; ToDo: It would be better to know what has been seen, but when OpenAI assistants go away this is going to be necessar always anyway!
          (tell-interviewer history iviewr-agent cid)
          (when-let [eads-id (db/get-active-EADS-id pid cid)]
            (tell-interviewer (db/get-EADS-instructions eads-id) iviewr-agent cid))

          (loop [ctx ctx] ; ToDo: currently not used!
            (let [{:keys [force-stop? exhausted? change-eads? new-eads-id cid old-cid new-cid?] :as state-change} (ork-review pid cid)]
              (if (or exhausted? force-stop?)
                (resume-exit state-change ctx)
                (let [eads-id (or new-eads-id (db/get-active-EADS-id pid cid))]
                  (when new-cid? (resume-update-db-cid pid cid old-cid))
                  (when change-eads?
                    (resume-update-db-eads pid cid new-eads-id)
                    (tell-interviewer (db/get-EADS-instructions new-eads-id) iviewr-agent cid))
                  ;; q-and-a does a SUPPLY-QUESTION and returns a vec of msg objects suitable for db/add-msg.
                  (let [conversation (q-and-a iviewr-agent pid cid ctx)
                        expert-response (-> conversation last :full-text) ; ToDo: This assumes the last is meaningful.
                        ;; Expect a DATA-STRUCTURE-REFINEMENT message to be returned from this call.
                        iviewr-response (if (and @mocking? (not (continue-mocking?)))
                                          {:message-type :mocking-complete}
                                          (tell-interviewer {:message-type :INTERVIEWEES-RESPOND :response expert-response}
                                                            iviewr-agent cid {:tries 2 :test-fn ai-response2clj}))]
                    (update-db-conversation! pid cid conversation)
                    (resume-post-ui-actions iviewr-response pid cid ctx) ; show graphs and tables, tell humans to switch conv.
                    ;; Use question-eads-id to ensure budget is decremented against the EADS that provided the question
                    (resume-post-db-actions! iviewr-response pid cid eads-id)) ; associate DS refinement with msg.
                  (recur (clear-ctx-ephemeral ctx)))))))
        (finally (ws/send-to-client {:dispatch-key :interviewer-busy? :value false :client-id client-id})))
      (find-appropriate-conv-and-redirect ctx))))

;;;------------------------------------ Starting and stopping --------------------------------
(defn init-iviewers!
  []
  (ws/register-ws-dispatch :resume-conversation resume-conversation))

(defstate iviewrs
  :start (init-iviewers!))
