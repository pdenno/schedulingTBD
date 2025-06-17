(ns scheduling-tbd.iviewr.ork
  "Code for working with the orchestrator agent."
  (:require
   [clojure.edn                   :as edn]
   [clojure.pprint                :refer [pprint cl-format]]
   [clojure.spec.alpha            :as s]
   [datahike.api                  :as d]
   [scheduling-tbd.agent-db       :as adb :refer [agent-log]]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.iviewr.eads] ; for mount
   [scheduling-tbd.sutil          :refer [connect-atm clj2json-pretty elide output-struct2clj]]
   [taoensso.telemere             :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(defn ensure-ork!
  "Create a orchstrator using ordinary rules for shared-assistant agents.
   This is typically called when the project starts. It updates :project/agents in the project DB.
   It is necessary to get a new orchestrartor thread for the project if you haven't changed the ork
   but want to start from scratch on the project."
  ([pid] (ensure-ork! pid {}))
  ([pid opts]
   (adb/ensure-agent! {:base-type :orchestrator-agent :pid pid} opts)))

(defn ^:admin refresh-ork!
  "I use this at the REPL to create a new ork in the system DB.
   That is enough to cause it to be reused in NEW projects."
  []
  (adb/ensure-agent! :orchestrator-agent #_{:force-new? true}))

;;; ToDo: Find a better home for this.
(defn challenge-key2sentence
  "Using a key from :process/warm-up-with-challenges, provides sentences describing the given scheduling challenges."
  [ckey]
  (assert (keyword? ckey))
  (let [defs
        {:raw-material-uncertainty "They sometimes don't have the raw material they need to make what they want to make."
         :demand-uncertainty "They are uncertain what to make because they are uncertain what customers need."
         :delivery-schedules "They are having problems meeting delivery promise dates."
         :variation-in-demand "They have slow periods and very busy periods. This is often the case, for example, when demand has seasonality."
         :planned-maintenance "They need to accommodate equipment maintenance schedules in their production schedules."
         :resource-assignment "They need to reserve several resources for simultaneous use."
         :equipment-changeover "The time it takes to change equipment setting and tooling is considerable."
         :equipment-availability "They struggle with equipment breakdowns."
         :equipment-utilization "They have expensive equipment that they would like to be able to use more."
         :worker-availability "They struggle with shortages of workers."
         :skilled-worker-availability "This is a specific subtype of 'worker-availability' where they suggest that matching worker skills and/or certification to the process is the challenge."
         :bottleneck-processes "The pace of their production is throttled by just a few processes."
         :process-variation "They have many different processes for making roughly the same class of products."
         :product-variation "They have many different products to make."
         :meeting-KPIs "They mention key performance indicators (KPIs) or difficulty performing to them."}]
    (str (name ckey) " : " (ckey defs))))

(defn max-ds
  "Return a map where keys are EADS ids and values are the EADS data structure from the
   highest :message/id of messages pursuing that EADS."
  [pid cid]
  (let [msgs (->> cid
                  (db/get-conversation pid)
                  :conversation/messages
                  (filter #(contains? % :message/EADS-data-structure)))
        msg-grps (-> (group-by :message/pursuing-EADS msgs) (dissoc nil))]
    (reduce-kv (fn [m k v]
                 (assoc m k (-> (apply max-key :message/id v)
                                :message/EADS-data-structure
                                edn/read-string)))
               {} msg-grps)))

(defn add-ds-to-activities
  "Iterate through the activities and just before changing to new pursuing-EADS add the summary DS for the last one."
  [pid activities]
  (letfn [(make-ds-entry [eads-id] {:summary-DS (db/get-summary-ds pid eads-id)})]
    (let [result (atom [])
          current-eads (atom nil)]
      (doseq [act activities]
        (if-let [eads-id (:pursuing-EADS act)]
          (when (not= @current-eads eads-id)
            (if @current-eads
              (swap! result into [(make-ds-entry @current-eads) act])
              (swap! result conj act))
            (reset! current-eads eads-id))
          (swap! result conj act)))
      (if @current-eads
        (conj @result (make-ds-entry @current-eads))
        @result))))

(defn conversation-activity
  "Create a chronologically-ordered vector of activities for the argument conversation, including q/a pairs,
   changes in the EADS being pursued, and the EADS data structure resulting."
  [pid]
  (let [msgs
        (reduce (fn [r cid]
                  (into r (->> (db/get-conversation pid cid)
                               :conversation/messages
                               (filter #(contains? % :message/answers-question))
                               (sort-by :message/id)
                               (mapv #(if-let [q-id (:message/answers-question %)]
                                        (assoc % :message/q-a-pair
                                               (let [q-msg (db/get-msg pid cid q-id)]
                                                 {:question (cond-> (:message/content q-msg)
                                                              (contains? q-msg :message/table) (str (:message/table q-msg)))
                                                  :answer   (cond-> (:message/content %)
                                                              (contains? % :message/table) (str (:message/table %)))}))
                                        %)))))
                []
                [:process :data :resources :optimality])
        current-eads (atom nil)
        result (atom [])]
    ;; Create the ordered activities vector in result
    (doseq [m msgs]
      (let [{:message/keys [pursuing-EADS code code-execution q-a-pair]} m]
        (when (and pursuing-EADS (not= pursuing-EADS @current-eads)) ; changing EADS, write a new pursuing-EADS activity
          (swap! result conj {:pursuing-EADS pursuing-EADS})
          (swap! current-eads pursuing-EADS))
        (when q-a-pair (swap! result conj q-a-pair))
        (when code           (swap! result conj {:code code}))
        (when code-execution (swap! result conj {:code-execution code}))))
    (add-ds-to-activities pid @result)))


;;;   CONVERSATION-HISTORY Keys
;;;
;;;   | Keys                             | Usage                                                                                              |
;;;   |----------------------------------+----------------------------------------------------------------------------------------------------|
;;;   | scheduling-challenges            | A list of challenges they face (one sentence for each)                                             |
;;;   | scheduling-challenge-observation | A single sentence about their scheduling challenges not covered by the scheduling challenges list. |
;;;   | interviewee-type                 | whether the interviewees are human experts in the production domain or an LLM-base surrogates.     |
;;;   | activity                         | A chronological list of objects with the following keys.  (See next table).                        |
;;;
;;;   CONVERSATION-HISTORY.activity Keys
;;;
;;;   | Keys                   | Usage                                                                                                                      |
;;;   |------------------------+----------------------------------------------------------------------------------------------------------------------------|
;;;   | pursuing-EADS          | the name of the EADS instructions being pursued in the activity entries below it.                                          |
;;;   | question and answer    | Q/A pairs; that are the question asked by an interview and the answer to it that interviewees provided.                    |
;;;   | summary-DS             | the data structure (DS) of the type named by the pursuing-EADS. This DS summarizes what was inferred from the questioning. |
;;;   | minizinc               | minizinc code that was inferred from the conversation so far.                                                              |
;;;   | minizinc-results       | output from running the minizinc.                                                                                          |
;;;
;;;   These Q/A pairs (objects with 'question' and 'answer' keys referred to above) are all in pursuit of the goal of the EADS named in the
;;;   pursuing-EADS object that appears directly above these Q/A pairs.
;;;   The object directly following a 'summary-DS' object is usually another 'pursuing-EADS' object.

;;; ToDo: Needs minizinc execution
(defn conversation-history
  "Return a CONVERSATION-HISTORY message for use with the orchestrator with values translated to JSON.

   There are potentially six kinds of information in the 'activity' property of this message:
        1) a list of all EADS (their EADS-id) that have been completely discussed,
        2) question/answer pairs: which are objects that have 'question' and 'answer' properties, and sometimes an additional 'table' property, and
        3) data-structure: which are objects that have a 'data-structure' property, the value of which is the the most complete response any of your PROVIDE-EADS messages on which the interviewer has worked.
        4) minizinc: which is an object with the property 'minizinc' the value of which is the current scheduling system MiniZinc code.
        5) minizinc-execution: which describes that results of executing the minizinc code.
        6) scheduling-challenges: an enumeration of scheduling challenge terms evident from discussion with the interviewees. These terms can be any of the following:

   We don't include all question/answer pairs, only those since our last C-H message to the orchestrator.
   (And we check that there isn't a new thread since that message.)
   Likewise, we only include the current data-structure if the orchestrator has been around for earlier C-H messages."
  [pid]
  (if (db/project-exists? pid)
    (let [warm-up (db/get-summary-ds :sur-craft-beer :process/warm-up-with-challenges)
          observation (:one-more-thing warm-up)
          challenges (->> warm-up
                          :scheduling-challenges
                          (map keyword)
                          (mapv challenge-key2sentence))
          itype (if (some #(= (first %) 'surrogate) (db/get-claims pid)) :surrogate :human)
          activities (conversation-activity pid)]
      (when (some nil? challenges) (log! :warn "nil scheduling challenge."))
      (cond-> {:message-type :CONVERSATION-HISTORY}
        (not-empty challenges)     (assoc :scheduling-challenges challenges)
        (not-empty observation)    (assoc :scheduling-challenge-observation observation)
        true                       (assoc :interviewee-type itype)
        (not-empty activities)     (assoc :activity activities)))
    {:message-type :CONVERSATION-HISTORY
     :scheduling-challenges []
     :activity []}))

(s/def ::ork-msg map?) ; ToDo: Write specs for ork messages.

(defn tell-ork
  "Send a message to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [msg ork-agent]
  (when-not (s/valid? ::ork-msg msg) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid ork msg:\n" (with-out-str (pprint msg)))))
  (log! :info (-> (str "Ork told: " msg) (elide 150)))
  (agent-log (cl-format nil "{:log-comment \"[ork manager] (tells ork):~%~A\"}"
                  (with-out-str (pprint msg))))
  (let [msg-string (clj2json-pretty msg)
        res (-> (adb/query-agent ork-agent msg-string {:tries 2}) output-struct2clj)
        res (cond-> res
              (contains? res :message-type)              (update :message-type keyword)
              (= (:message-type res) :PURSUE-EADS)       (update :EADS-id keyword))]
    (log! :info (-> (str "Ork returns: " res) (elide 150)))
    (agent-log (cl-format nil "{:log-comment \"[ork manager] (receives response from ork):~%~A\"}"
                    (with-out-str (pprint res))))
    res))

(defn get-new-EADS-id
  "Update the project's orchestrator with CONVERSATION-HISTORY and do a SUPPLY-EADS request to the ork.
   If the ork returns {:message-type 'PURSUE-EDS', :EADS-id 'exhausted'} return nil to the caller,
   otherwise, return the eads-instructions-id keyword."
  [pid]
  (let [ork (ensure-ork! pid)
        old-tid (d/q '[:find ?tid .
                       :where
                       [?e :agent/base-type :orchestrator-agent]
                       [?e :agent/thread-id ?tid]]
                     @(connect-atm pid))]
    (when-not (= old-tid (:tid ork))
      (agent-log (cl-format nil "{:log-comment \"Project's ork thread has been updated; provide comprehensive history.\"}"
                            {:console? true :level :warn})))
    ;; ToDo: For the time being, I always provide the complete history for :process (the cid).
    (let [pursue-msg (tell-ork (conversation-history pid) ork) ; conversation-history defaults to :all (all cids).
          eads-instructions-id (-> pursue-msg :EADS-id keyword)]
      (if ((db/system-EADS?) eads-instructions-id)
        eads-instructions-id
        ;; Otherwise probably :exhausted. Return nil
        (agent-log (cl-format nil "{:log-comment \"Exhausted or invalid PURSUE-EADS message: ~A\"}" pursue-msg)
                   {:console? true :level :info})))))
