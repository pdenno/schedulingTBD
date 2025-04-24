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
  (let [agent (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent)
                                     (assoc :pid pid)))]))

;;; ToDo: Check the :conversation/ork-tid versus the actual (once this doesn't provide the whole thing; so that you don't have to provide the whole thing to same ork.)
;;; ToDo: Currently this is very similar to inv/conversation-history.
;;; ToDo: Needs minizinc execution
(defn conversation-history
  "Return a CONVERSATION-HISTORY message for use with the orchestrator.
   There are potentially five kinds of information in the 'activity' property of this message:
        1) question/answer pairs: which are objects that have 'question' and 'answer' properties, and
        2) data-structure: which are objects that have a 'data-structure' property, the value of which is the the most complete response any of your PROVIDE-EADS messages on which the interviewer has worked.
        3) minizinc: which is an object with the property 'minizinc' the value of which is the current scheduling system MiniZinc code.
        4) minizinc-execution: which describes that results of executing the minizinc code.
        5) scheduling-challenges: an enumeration of scheduling challenge terms evident from discussion with the interviewees. These terms can be any of the following:

   We don't include all question/answer pairs, only those since our last C-H message to the orchestrator.
   (And we check that there isn't a new thread since that message.)
   Likewise, we only include the current data-structure if the orchestrator has been around for earlier C-H messages."
  [pid cid]
  (let [msgs (-> (db/get-conversation pid cid) :conversation/messages)
        _zippy (reset! diag msgs)
        answers (filter #(contains? % :message/answers-question) msgs)
        challenges (->> (db/get-claims pid)
                        (filter #(uni/unify '(scheduling-challenge ?pid ?challenge) %))
                        (mapv (fn [[_ _ x]] (name x))))
        ds (db/get-EADS-ds pid cid)
        code (db/get-code pid)]
    (cond-> {:message-type "CONVERSATION-HISTORY"
             :interview-agent (name cid)}
      (not-empty answers)             (assoc :activity (mapv (fn [answer]
                                                               {:question (-> (db/get-msg pid cid (:message/answers-question answer)) :message/content)
                                                                :answer  (-> answer :message/content)})
                                                             answers))

      (not-empty challenges)          (update :activity conj {:scheduling-challenges challenges})
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
    @result-atm))

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
  "If there is another relevant EADS for Update the project's orchestrator with CONVERSATION-HISTORY and do a SUPPLY-EADS request. Return the EADS to the interviewer.
  [pid cid]



;;; ====================================================================================================================================================================================
;;; ================================================================== I think this stuff goes away once the orchestrator is implemented. ==============================================
;;; ====================================================================================================================================================================================

;;; ToDo: I'm currently not handling anything but flow-shop
#_(def process-eads2file
  {:FLOW-SHOP-SCHEDULING-PROBLEM      "EADS/flow-shop.edn"
   :RESOURCE-ASSIGNMENT-PROBLEM       nil
   :PROJECT-SCHEDULING-PROBLEM        nil
   :JOB-SHOP-SCHEDULING-PROBLEM       "EADS/flow-shop.edn"
   :SINGLE-MACHINE-SCHEDULING-PROBLEM nil})


#_(s/def :process/EADS-keyword (fn [key] (#{:FLOW-SHOP-SCHEDULING-PROBLEM
                                            :RESOURCE-ASSIGNMENT-PROBLEM
                                            :PROJECT-SCHEDULING-PROBLEM
                                            :JOB-SHOP-SCHEDULING-PROBLEM
                                            :SINGLE-MACHINE-SCHEDULING-PROBLEM}
                                        key)))

;;;  "Create a message of type EADS for the given PHASE-1-CONCLUSION"
#_(defmethod eads-response!
  :process [_tag pid cid {:keys [problem-type _cyclical?] :as _iviewr-response}]
  (let [k (-> problem-type str/upper-case keyword)]
    (s/assert :process/EADS-keyword k)
    (if-let [resource (get process-eads2file k)]
      (let [eads (-> resource io/resource slurp edn/read-string)]
        (db/put-eads! pid cid (str eads))
        {:message-type "EADS"
         :EADS (with-out-str (clojure.data.json/pprint eads))})
      (log! :error (str "No EADS for problem type " k)))))
