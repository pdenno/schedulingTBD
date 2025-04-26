(ns scheduling-tbd.interviewing.ork
  "Code for working with the orchestrator agent."
  (:require
   [clojure.core.unify            :as uni]
   [clojure.edn                   :as edn]
   [clojure.set                   :as set]
   [datahike.api                  :as d]
   [scheduling-tbd.agent-db       :as adb :refer [agent-log]]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.sutil          :refer [connect-atm]]
   [taoensso.telemere             :as tel :refer [log!]]))

(def diag (atom nil))

(defn ensure-ork
  "Create a orchstrator using ordinary rules for shared-assistant agents.
   This is typically called when the project starts. It updates :project/agents in the project DB."
  [pid]
  (adb/ensure-agent! {:base-type :orchestrator-agent :pid pid #_#_:force-new? true})) ; <===================================== :force-new? is temporary.


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

(if (= cid :all)
  (reduce (fn [res cid] (into res (-> cid (db/get-conversation pid) :conversation/messages))) [] [:process :data :resources :optimality])


;;; ToDo: Check the :conversation/ork-tid versus the actual (once this doesn't provide the whole thing; so that you don't have to provide the whole thing to same ork.)
;;; ToDo: Currently this is very similar to inv/conversation-history.
;;; ToDo: Needs minizinc execution
(defn conversation-history
  "Return a CONVERSATION-HISTORY message for use with the orchestrator.
   The
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
             (let [msgs (->> cid (db/get-conversation pid) :conversation/messages)
                   msgs-ds      (filter #(contains? % :message/data-structure) msgs)
                   msgs-eads-id (filter #(contains? % :message/pursuing-EADS) msgs)
                   msgs-code    (filter #(contains? % :message/code) msgs)
                   msgs-answers (filter #(contains? % :message/answers-question) msgs)
                   q-a-pairs    (mapv (fn [answer]
                                        {:question (-> (db/get-msg pid cid (:message/answers-question answer)) :message/content)
                                         :answer  (-> answer :message/content)})
                                      msgs-answers)]

               ))]
     (let [challenges (->> (db/get-claims pid)
                           (filter #(uni/unify '(scheduling-challenge ?pid ?challenge) %))
                           (mapv #(nth % 2))
                           (mapv #(get scheduling-challenge2-description %)))
           activities (if (= :all cid)
                        (reduce (fn [res cid] (into res (c-h-cid cid))) [] [:process :data :resources :optimality])
                        (c-h-cid cid))]
       (when (some nil? challenges) (log! :warn "nil scheduling challenge."))
       (cond-> {:message-type "CONVERSATION-HISTORY"
              :scheduling-challenges challenges
              (not-empty answers)             (assoc :activity
       ds                              (update :activity conj {:data-structure ds})
       (not-empty code)                (update :activity conj {:minizinc code}))))

  (defn collect-keys
    [obj]
    (let [result-atm (atom #{})]
      (letfn [(ck [obj]
                (cond (map? obj)            (doseq [[k v] obj]
                                              (swap! result-atm conj k)
                                              (ck v))
                      (vector? obj)         (doseq [o obj] (ck o))))]
        (ck obj))
      @result-atm)))

(def EADS-ignore-keys "Ignore these in comparing EADS to a data structure"  #{:comment :val :EADS-id})

;;; ToDo: At some point I hope to use the spec (or Malli) but for now, I look at the EADS and see if there are values for all properties SOMEWHERE in the data structure.
;;; ToDo: More thoughts on the above ToDo: It might be valuable to list what keys are optional. The specs can do this, of course, but can I use them?
;;;       This would also be
(defn eads-complete?
  "Check whether all the properties of the EADS are used somewhere in the data structure."
  [pid cid eads-id]
  (let [dstruct (-> (db/get-EADS-dstructs pid cid) (max-key :message/id) :message/EADS-data-structure)
        ds-eads-ref (:based-on-EADS dstruct)
        ds-keys (-> dstruct (max-key :message/id) edn/read-string collect-keys)
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
        empty?)))

(defn get-new-EADS
  "Update the project's orchestrator with CONVERSATION-HISTORY and do a SUPPLY-EADS request to the ork.
   If the ork returns {CONVEY-EDS :exhausted} return nil to the caller."
  [pid cid]
  (let [old-tid (d/q '[:find ?tid .
                       :where
                       [?e :agent/base-type :orchestrator-agent]
                       [?e :agent/thread-id ?tid]]
                     @(connect-atm pid))
        [{:keys [aid tid]} (ensure-ork pid)]]
    (when-not (= old-tid tid)
      (log! :warn "Project's ork thread has been updated; provide comprehensive history."))
    ;; ToDo: For the time being, I always provide the complete history for :process (the cid).
    ;;       'comprehensive history' probably covers all conversations.
