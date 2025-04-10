(ns scheduling-tbd.interviewing.ork
  "Code for working with the orchestrator agent."
  (:require
   [scheduling-tbd.agent-db       :as adb :refer [agent-log]]
   [scheduling-tbd.db             :as db]
   [taoensso.telemere             :as tel :refer [log!]]))

(defn ensure-ork
  "Create a orchstrator using ordinary rules for shared-assistant agents.
   This is typically called when the project starts. It updates :project/agents in the project DB."
  [pid]
  (adb/ensure-agent! (-> (get @adb/agent-infos :orchestrator-agent)
                         (assoc :pid pid))))

;;; ToDo: Check the :conversation/ork-tid versus the actual
(defn ork-conversation-history
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
  [& _]
  :nyi)

;;; ToDo: Need to rethink how I'm handling EADS. I think they belong in the system DB, not resources.
;;;       That way, I can search them, point to specs to test how far along things are, etc.
(defn active-EADS!
  "Determine which EADS (if any) is being pursued for the given conversation.
   If none is being pursued, or what has been pursued is completed, determine which should be pursued next.
   This involves apprising the orchestrator of the current situation with the conversation using ork-conversation-history.
   It also sometimes involves updating the conversation with a new EADS. "
  [pid cid]
  (let [eads-maps (db/get-EADS-ds pid cid)
        max-msg (when (not-empty eads-maps)
                  (let [id (->> eads-maps (map :message/id) (apply max))]
                    (some #(when (= (:message/id %) id) %) eads-maps)))
        ds (:message/EADS-data-structure max-msg)]))
