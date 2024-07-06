(ns scheduling-tbd.domain.process.p-ops
  "Planning operators for the process interview"
  (:require
   [clojure.edn                               :as edn]
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.domain.process.p-interview :as inv]
   [scheduling-tbd.op-utils                   :as ou :refer [chat-pair db-action defoperator defaction make-human-project surrogate?]]
   [scheduling-tbd.sutil                      :as sutil :refer [register-planning-domain find-fact]]
   [scheduling-tbd.web.websockets             :as ws]
   [taoensso.timbre                           :as log]))

;;; Place this in any file where you are developing the process interview so updates will be found.
(register-planning-domain :process  (-> "data/planning-domains/process-interview.edn" slurp edn/read-string))

(def ^:diag diag (atom nil))

;;; ----- :!initial-process-question
(def ^:diag intro-prompt
  [{:msg-text/string "Describe your most significant scheduling problem in a few sentences"}
   {:human-only [{:msg-text/string " or "}
                 {:msg-link/uri "http://localhost:3300/learn-more"
                  :msg-link/text "learn more about how this works"}]}
   {:msg-text/string "."}])

;;;=================================================== Operators ======================================
;;; Operator db-actions update state in the db (db/add-plannin-state) and return nil.
;;; The update does not include the call to operator-update-state, which applies the d-list and a-list.
;;; operator-update-state is called by the planner, plan/update-planning.

(defoperator :!initial-process-question [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      "Describe your most significant scheduling problem in a few sentences."
                      (str "Describe your most significant scheduling problem in a few sentences or "
                           "<a href=\"http://localhost:3300/learn-more\">learn more about how this works</a>."))]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:initial-process-question]) db-action)))

;;; (op/db-action (assoc op/example :client-id (ws/recent-client!)))
(defaction :!initial-process-question [{:keys [state response client-id] :as _obj}]
  (log/info "*******db-action (!initial-process-question): response =" response "state =" state)
  (let [surrogate? (surrogate? state)
        analysis-state (inv/analyze-intro-response response state)] ; return state props proj-id and proj-name if human, otherwise argument state.
    (when-not surrogate? (make-human-project analysis-state))
    ;;--------  Now human/surrogate can be treated nearly identically ---------
    (let [[_ pid]   (find-fact '(proj-id ?x) analysis-state)
          [_ pname] (find-fact '(proj-name ?x) analysis-state)
          pid (keyword pid)]
      (db/add-planning-state pid analysis-state)
      (db/add-msg pid :system (format "Great, we'll call your project %s." pname) [:informative])
      ;; Complete preliminary analysis in a parallel agent that, in the case of a human expert, works independently.
      (db/add-planning-state pid (inv/parallel-expert-prelim-analysis pid)))))

(defoperator :!cites-supply-issues? [{:keys [state response client-id] :as _obj}]
  (let [msg (str "Though you've cited a challenge with inputs (raw material, workers, or other resources), "
                 "we'd like to put that aside for a minute and talk about the processes that make product.")]
    (ws/send-to-chat {:promise? nil :client-id client-id :dispatch-key :tbd-says :msg msg})
    (let [[_ pid]   (find-fact '(proj-id ?x) state)]
      (db/add-msg pid :system msg)
      (ws/refresh-client client-id pid :process))))

;;; ----- :!query-process-steps ---------------------------------------------------------------------------------------------------------------------
(defoperator :!query-process-steps [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      (str "Please list the steps of your process, one per line in the order they are executed to produce a product, "
                           "so it looks like this:\n"
                           "1. (the first step)\n"
                           "2. (the second step)...\n")
                      "Select the process steps from the list on the right that are typically part of your processes. \n (When done hit \"Submit\".)")]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:query-process-steps]) db-action)))

(defaction :!query-process-steps [{:keys [pid response client-id] :as obj}]
  ;; Nothing to do here but update state from a-list.
  (reset! diag obj)
  (log/info "!yes-no-process-steps (action): response =" response)
  (let [more-state (inv/analyze-process-steps-response obj)
        new-code (inv/mzn-process-steps more-state)
        minizinc-enum-announce
        (str "Okay, we now know enough to get started on a MiniZinc solution. "
             "In the code pane (upper right of the app) we added a <a href=\"http://localhost:3300/mzn-enum\">MiniZinc enum</a>. "
             "The 'enum values' name the steps of your process in the order they are executed for each product.")]
    (db/add-planning-state pid more-state)
    (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
    (db/put-code pid new-code)
    ;; ToDo: This should really be just for humans. Maybe use the DB's message tags to decide that. (or to decide what to send).
    (db/add-msg pid :system minizinc-enum-announce [:info-to-user :minizinc])
    (ws/refresh-client client-id pid :process)
    more-state))

;;; ----- :!query-process-durs ---------------------------------------------------------------------------------------------------------------------
(defoperator :!query-process-durs [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      (format (str "I suppose processing times for each of the steps you just mentioned might vary from product to product. "
                                   "But generally speaking, how long does each step take? "
                                   "Please produce a list just like the one you did for process steps, one process per line, but add to it the typical processing time "
                                   "so it looks like this:\n"
                                   "1. %s (some amount of time)\n"
                                   "2. %s (some amount of time)...")
                              (-> (find-fact '(process-step ?proj 1 ?process) state) (nth 3))
                              (-> (find-fact '(process-step ?proj 2 ?process) state) (nth 3)))
                      "Provide typical process durations for the tasks on the right.\n(When done hit \"Submit\".)")]
    (-> obj (assoc :agent-query agent-query) (chat-pair [:process-durs]) db-action)))

(defaction :!query-process-durs [{:keys [client-id response pid] :as obj}]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (let [more-state (inv/analyze-process-durs-response obj)]
    (ws/refresh-client client-id pid :process)
    (db/add-planning-state pid more-state)))
