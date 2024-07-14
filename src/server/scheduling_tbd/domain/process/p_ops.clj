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

;;; ----- :describe-challenge
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

(defoperator :!describe-challenge [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      "Describe your most significant scheduling problem in a few sentences."
                      (str "Describe your most significant scheduling problem in a few sentences or "
                           "<a href=\"http://localhost:3300/learn-more\">learn more about how this works</a>."))]
    (-> obj
        (assoc :agent-query agent-query)
        (chat-pair {:msg-keys [:initial-process-question]})
        db-action)))

;;; (op/db-action (assoc op/example :client-id (ws/recent-client!)))
(defaction :!describe-challenge [{:keys [state response client-id] :as _obj}]
  (log/info "*******db-action (!describe-challenge): response =" response "state =" state)
  (let [surrogate? (surrogate? state)
        analysis-state (inv/analyze-intro-response response state)] ; return state props proj-id and proj-name if human, otherwise argument state.
    (when-not surrogate? (make-human-project analysis-state))
    ;;--------  Now human/surrogate can be treated nearly identically ---------
    (let [[_ pid]   (find-fact '(proj-id ?x) analysis-state)
          [_ pname] (find-fact '(proj-name ?x) analysis-state)
          pid (keyword pid)]
      (db/add-planning-state pid analysis-state)
      (db/add-msg pid :system (format "Great, we'll call your project %s." pname) [:informative])
      (ws/refresh-client client-id pid :process))))

;;; Above :!describe-challenges may add-planning-state (cites-raw-material-challenge ?p).
(defoperator :!remark-raw-material-challenge [{:keys [pid state response client-id] :as _obj}]
  (let [msg (str "Though you've cited a challenge with inputs (raw material, workers, or other resources), "
                 "we'd like to put that aside for a minute and talk about the processes that make product.")]
    (ws/send-to-chat {:promise? nil :client-id client-id :dispatch-key :tbd-says :msg msg})
    (db/add-msg pid :system msg)))

;;; ----- :!describe-process -----------------------------------------------------------------
(defoperator :!describe-process [obj]
  (let [query "Please briefly describe your production processes."]
    (-> obj
        (assoc :agent-query query)
        chat-pair)))

;;; ----- :!query-product-or-service ---------------------------------------------------------
(defoperator :!query-product-or-service [obj]
  (let [query (str "Would you characterize your company's work as primarily providing a product or a service? "
                   "Respond respectively with either the single word PRODUCT or SERVICE.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair {:tries 2})
        db-action)))

(defaction :!query-product-or-service [{:keys [pid client-id response] :as _obj}]
  (let [new-fact (cond (re-matches #".*(?i)PRODUCT.*" response) (list 'provides-product pid)
                       (re-matches #".*(?i)SERVICE.*" response) (list 'provides-service pid)
                       :else                                    (list 'fails-query 'product-or-service? pid))]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-production-mode ---------------------------------------------------------
(defoperator :!query-production-mode [obj]
  (let [query (str "Three commonly recognized ways of production are termed MAKE-TO-STOCK, MAKE-TO-ORDER, and ENGINEER-TO-ORDER. "
                   "In MAKE-TO-STOCK you make product to replenish inventory based on forecasted demand. "
                   "In MAKE-TO-ORDER you make product because a customer has specifically asked you to, and the customer has described characteristics of the product in your own terminology, perhaps using your catalog of offerings. "
                   "ENGINEER-TO-ORDER is something like MAKE-TO-ORDER but here the customer also expects you to do some creative problem solving to meet their need. "
                   "For example, a commercial aircraft might be ENGINEER-TO-ORDER because though the customer may have specified the engine type and seating capacity it wants, "
                   "it is relying on you to determine how to best accommodate the engine and arrange the seats. "
                   "Other examples of ENGINEER-TO-ORDER include general contracting for building construction, film production, event planning, and 3rd party logisistics. "
                   "Respond with just one of the terms MAKE-TO-STOCK, MAKE-TO-ORDER or ENGINEER-TO-ORDER according to which most accurately describes your mode of production. ")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair #_{:tries 2}) ; <============================== Not sure tries>1 makes sense unless :preprocess-fn and :test are specified.
        db-action)))

(defaction :!query-production-mode [{:keys [pid response] :as _obj}]
  (let [new-fact (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" response)        (list 'production-mode pid 'make-to-stock)
                       (re-matches #".*(?i)MAKE-TO-ORDER.*" response)        (list 'production-mode pid 'make-to-order)
                       (re-matches #".*(?i)ENGINEER-TO-ORDER.*" response)    (list 'production-mode pid 'engineer-to-order)
                       :else                                                 (list 'fails-query 'production-mode pid))]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-activity-location-type ---------------------------------------------------------
(defoperator :!query-activity-location-type [obj]
  (let [query (str "Some work, for example factory work, must be performed in a specially designed facility. "
                   "Other work, like cutting down a tree, can only be performed at a location designated by the customer. "
                   "Are the processes you describe things that must be performed at your facility, or are they things that must be done at the customer's site? "
                   "Respond respectively with either the single term OUR-FACILITY or CUSTOMER-SITE.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair #_{:tries 2})
        db-action)))

;;; ----- :!query-activity-location-type ---------------------------------------------------------
(defaction :!query-activity-location-type [{:keys [pid response] :as _obj}]
  (let [new-fact (cond (re-matches #".*(?i)OUR-FACILITY.*" response)  (list 'has-production-facility pid)
                       (re-matches #".*(?i)CUSTOMER-SITE.*" response) (list 'performed-at-customer-site pid)
                       :else                                          (list 'fails-query 'facility-vs-site pid))]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-shop-type ---------------------------------------------------------
(defoperator :!query-shop-type [obj]
  (let [query (str "A FLOW-SHOP is a production system designed so that all jobs follows the same sequence of steps through production resources. "
                   "A JOB-SHOP is a production system where each job might follow its own route, depending on its unique requirements. "
                   "Is the process you described more like a flow-shop or a job-shop? "
                   "Respond respectively with either the single term FLOW-SHOP or JOB-SHOP.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair #_{:tries 2})
        db-action)))

(defaction :!query-shop-type [{:keys [pid response] :as _obj}]
  (let [new-fact (cond (re-matches #".*(?i)FLOW-SHOP.*" response)  {:preds '[(flow-shop ?x)]}
                       (re-matches #".*(?i)JOB-SHOP.*"  response)  {:preds '[(job-shop ?x)]}
                       :else                                       {:preds '[(fails-query flow-vs-job ?x)]})]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-process-steps. This is for flow shops. ------------------------------------------------
(defoperator :!query-process-steps [{:keys [state] :as obj}]
  (let [agent-query (if (surrogate? state)
                      (str "Please list the steps of your process, one per line in the order they are executed to produce a product, "
                           "so it looks like this:\n"
                           "1. (the first step)\n"
                           "2. (the second step)...\n")
                      "Select the process steps from the list on the right that are typically part of your processes. \n (When done hit \"Submit\".)")]
    (-> obj
        (assoc :agent-query agent-query)
        (chat-pair {:msg-keys [:query-process-steps]})
        db-action)))

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
    (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
    (db/put-code pid new-code)
    ;; ToDo: This should really be just for humans. Maybe use the DB's message tags to decide that. (or to decide what to send).
    (db/add-msg pid :system minizinc-enum-announce [:info-to-user :minizinc])
    (ws/refresh-client client-id pid :process)
    (db/add-planning-state pid more-state)))

;;; ----- :!query-process-durs -----------------------------------------------------------
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
    (-> obj
        (assoc :agent-query agent-query)
        (chat-pair {:msg-keys [:process-durs]})
        db-action)))

(defaction :!query-process-durs [{:keys [client-id response pid] :as obj}]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (let [more-state (inv/analyze-process-durs-response obj)]
    (ws/refresh-client client-id pid :process)
    (db/add-planning-state pid more-state)))
