(ns scheduling-tbd.interviewing.ork
  "Code for working with the orchestrator agent."
  (:require
   [clojure.core.unify            :as uni]
   [clojure.edn                   :as edn]
   [clojure.pprint                :refer [pprint]]
   [clojure.set                   :as set]
   [clojure.spec.alpha            :as s]
   [datahike.api                  :as d]
   [scheduling-tbd.agent-db       :as adb :refer [agent-log]]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.interviewing.eads] ; for mount
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
   (adb/ensure-agent! nil (merge {:pid pid :base-type :orchestrator-agent} opts))))

;;; ToDo: Find a better home for this.
(def scheduling-challenge2-description
  "ork/conversation-history provides sentences describing scheduling challenges."
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
   :meeting-KPIs "They mention key performance indicators (KPIs) or difficulty performing to them."})

;;; ToDo: Check the :conversation/ork-tid versus the actual (once this doesn't provide the whole thing; so that you don't have to provide the whole thing to same ork.)
;;; ToDo: Currently this is very similar to inv/conversation-history.
;;; ToDo: Needs minizinc execution
(defn conversation-history
  "Return a CONVERSATION-HISTORY message for use with the orchestrator with values translated to JSON.

   There are potentially five kinds of information in the 'activity' property of this message:
        1) a list of all EADS (their EADS-id) that have been completely discussed,
        2) question/answer pairs: which are objects that have 'question' and 'answer' properties, and
        3) data-structure: which are objects that have a 'data-structure' property, the value of which is the the most complete response any of your PROVIDE-EADS messages on which the interviewer has worked.
        4) minizinc: which is an object with the property 'minizinc' the value of which is the current scheduling system MiniZinc code.
        5) minizinc-execution: which describes that results of executing the minizinc code.
        6) scheduling-challenges: an enumeration of scheduling challenge terms evident from discussion with the interviewees. These terms can be any of the following:

   We don't include all question/answer pairs, only those since our last C-H message to the orchestrator.
   (And we check that there isn't a new thread since that message.)
   Likewise, we only include the current data-structure if the orchestrator has been around for earlier C-H messages."
  ([pid] (conversation-history pid :all))
  ([pid cid]
   (assert (#{:all :process :data :resources :optimality} cid))
   (letfn [(c-h-cid [cid]
             (let [msgs (->> cid
                             (db/get-conversation pid)
                             :conversation/messages
                             (sort-by :message/id)
                             (mapv #(if-let [q-id (:message/answers-question %)]
                                      (assoc % :message/q-a-pair
                                             (let [q-msg (db/get-msg pid cid q-id)]
                                               {:question (:message/content q-msg)
                                                :answer   (:message/content %)})) ; ToDo: Might want to skip the answer if it is long.
                                      %)))
                   current-eads (atom nil)
                   result (atom [])]
               (doseq [m msgs]
                 (let [{:message/keys [id pursuing-EADS EADS-data-structure code code-execution q-a-pair]} m]
                   (when (and pursuing-EADS (not= pursuing-EADS @current-eads))
                     (reset! current-eads pursuing-EADS)
                     (swap! result conj {:pursuing-EADS (name pursuing-EADS)}))
                   (when q-a-pair (swap! result conj q-a-pair))
                   ;; We only want the last d-s for this pursuing-EADS.
                   (when (and EADS-data-structure
                              (= id (:message/id (apply max-key :message/id (filter #(= pursuing-EADS (:message/pursuing-EADS %)) msgs)))))
                     (swap! result conj {:data-structure (clj2json-pretty EADS-data-structure)}))
                   (when code           (swap! result conj {:code code}))
                   (when code-execution (swap! result conj {:code-execution code}))))
               @result))]
     (let [challenges (->> (db/get-claims pid)
                           (filter #(uni/unify '(scheduling-challenge ?pid ?challenge) %))
                           (mapv #(nth % 2))
                           (mapv #(get scheduling-challenge2-description %)))
           activities (if (= :all cid)
                        (reduce (fn [res cid] (into res (c-h-cid cid))) [] [:process :data :resources :optimality])
                        (c-h-cid cid))]
       (when (some nil? challenges) (log! :warn "nil scheduling challenge."))
       (cond-> {:message-type "CONVERSATION-HISTORY"}
         challenges              (assoc :scheduling-challenges challenges)
         (not-empty activities)  (assoc :activity activities))))))

(defn collect-keys
  [obj]
  (let [result-atm (atom #{})]
    (letfn [(ck [obj]
              (cond (map? obj)            (doseq [[k v] obj]
                                            (swap! result-atm conj k)
                                            (ck v))
                    (vector? obj)         (doseq [o obj] (ck o))))]
      (ck obj))
    @result-atm))

(def EADS-ignore-keys "Ignore these in comparing EADS to a data structure"  #{:comment :val :EADS-id})

;;; ToDo: At some point I hope to use the spec (or Malli) but for now I look at the EADS and see if there are values for all properties SOMEWHERE in the data structure.
;;; ToDo: More thoughts on the above ToDo: It might be valuable to list what keys are optional. The specs can do this, of course, but can I use them?
;;;       This would also be
;;;(ork/eads-complete :plate-glass-ork :process
(defn EADS-complete?
  "Check whether the argument eads is complete.
   Some interviewers are instructed to set a property 'exhausted?' to true (e.g. the orm interviewer does this),
   for other we check that all the properties of the EADS are used somewhere in the data structure."
  [pid cid eads-id]
  (let [dstruct (as-> (db/get-EADS-dstructs pid cid) ?x
                  (apply max-key :message/id ?x)
                  (:message/EADS-data-structure ?x)
                  (edn/read-string ?x))]
    (if (:exhausted? dstruct)
      true
      (let [ds-eads-ref (:based-on-EADS dstruct)
            ds-keys (collect-keys dstruct)
            eads-keys (-> (d/q '[:find ?str .
                                 :in $ ?eads-id
                                 :where
                                 [?e :EADS/id ?eads-id]
                                 [?e :EADS/msg-str ?str]]
                               @(connect-atm :system) eads-id)
                          edn/read-string
                          collect-keys)]
        (when-not (= eads-id ds-eads-ref)
          (log! :warn (str "Argument eads-id, " eads-id " does not match that of data structure, " ds-eads-ref)))
        (-> eads-keys
            (set/difference ds-keys)
            (set/difference EADS-ignore-keys)
            empty?)))))

(s/def ::ork-msg map?) ; ToDo: Write specs for ork messages.

(defn tell-ork
  "Send a message to an interviewer agent and wait for response; translate it.
   :aid and :tid in the ctx should be for the interviewer agent."
  [msg {:keys [ork-agent] :as ctx}]
  (when-not (s/valid? ::ork-msg msg) ; We don't s/assert here because old project might not be up-to-date.
    (log! :warn (str "Invalid ork msg: " (with-out-str (pprint msg)))))
  (log! :info (-> (str "Ork told: " msg) (elide 150)))
  (agent-log (str "[ork manager] (tells ork) " (with-out-str (pprint msg))))
  (let [msg-string (clj2json-pretty msg)
        res (-> (adb/query-agent ork-agent msg-string ctx) output-struct2clj)]
    (log! :info (-> (str "Ork returns: " res) (elide 150)))
    (agent-log (str "[ork manager] (receives response form ork) " (with-out-str (pprint res))))
    res))

;(s/def ::pursue-eads (s/keys :req-un [::message-type ::EADS-id]))
;(s/def ::message-type #(= % "PURSUE-EADS"))
;(def eads-id? (-> (db/list-system-EADS) set))
;(s/def ::EADS-id eads-id?)

(defn get-new-EADS-id
  "Update the project's orchestrator with CONVERSATION-HISTORY and do a SUPPLY-EADS request to the ork.
   If the ork returns {:message-type 'PURSUE-EDS', :EADS-id 'exhausted'} return nil to the caller,
   otherwise, return the :EADS-id as a keyword."
  [pid]
  (let [ork (ensure-ork! pid)
        old-tid (d/q '[:find ?tid .
                       :where
                       [?e :agent/base-type :orchestrator-agent]
                       [?e :agent/thread-id ?tid]]
                     @(connect-atm pid))]
    (when-not (= old-tid (:tid ork))
      (agent-log "Project's ork thread has been updated; provide comprehensive history."
                 {:console? true :level :warn}))
    ;; ToDo: For the time being, I always provide the complete history for :process (the cid).
    (tell-ork (conversation-history pid) {:ork-agent ork})
    (let [eads-msg (-> (tell-ork {:message-type "SUPPLY-EADS"} {:ork-agent ork})
                       (update :EADS-id keyword))]
      (if (s/valid? ::pursue-eads eads-msg)
        (:EADS-id eads-msg)
        (log! :error (str "Invalid PURSUE-EADS message: " eads-msg))))))
