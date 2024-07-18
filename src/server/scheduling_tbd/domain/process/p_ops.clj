(ns scheduling-tbd.domain.process.p-ops
  "Planning operators for the process interview"
  (:require
   [clojure.edn                               :as edn]
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.domain.process.p-interview :as inv]
   [scheduling-tbd.minizinc                   :as mzn]
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

(defoperator :!do-nothing [obj] obj)

;;;=================================================== Operators ========================================
;;; The operator's defoperator (expands to an ou/operator-meth method) takes a single argument that is a
;;; map of options, which includes keys  :state, and :tag (the :!whatever which names the operator.

;;; The operator's defaction (expands to an ou/ db-action method) updates state in the db (db/add-planning-state)
;;; and return nil. Further updating owing to the a/d lists of the operator occurs later in
;;; ou/operator-update-state, which is called by plan/update-planning.
(defoperator :!describe-challenge [{:keys [state] :as obj}]
  (let [query "Describe your most significant scheduling problem in a few sentences."]
    (-> obj
        (assoc :agent-query query)
        (chat-pair {:tags [:!describe-challenge]})
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
      (db/add-msg pid :system (format "Great, we'll call your project %s." pname) [:!describe-challenge :informative])
      (ws/refresh-client client-id pid :process))))

;;; Above :!describe-challenges may add-planning-state (cites-raw-material-challenge ?p).
(defoperator :!remark-raw-material-challenge [{:keys [pid client-id] :as _obj}]
  (let [info (str "Though you've cited a challenge with inputs (raw material, workers, or other resources), "
                 "we'd like to put that aside for a minute and talk about the processes that make product.")]
    (db/add-msg pid :system info [:!remark-raw-material-challenge :informative])
    (ws/refresh-client client-id pid :process)))

;;; ----- :!describe-process -----------------------------------------------------------------
(defoperator :!describe-process [obj]
  (let [query "Please briefly describe your production processes."]
    (-> obj
        (assoc :agent-query query)
        chat-pair {:tags :!describe-process})))

;;; ----- :!query-product-or-service ---------------------------------------------------------
(defoperator :!query-product-or-service [obj]
  (let [query (str "Would you characterize your company's work as primarily providing a product or a service? "
                   "Respond respectively with either the single word PRODUCT or SERVICE.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair {:tags [:query-product-or-service] :tries 2})
        db-action)))

(defaction :!query-product-or-service [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)PRODUCT.*" response) (list 'provides-product pid-sym)
                       (re-matches #".*(?i)SERVICE.*" response) (list 'provides-service pid-sym)
                       :else                                    (list 'fails-query 'product-or-service? pid-sym))]
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
        (chat-pair {:tags [:!query-production-mode] #_#_:tries 2}) ; <============================== Not sure tries>1 makes sense unless :preprocess-fn and :test are specified.
        db-action)))

(defaction :!query-production-mode [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" response)        (list 'production-mode pid-sym 'make-to-stock)
                       (re-matches #".*(?i)MAKE-TO-ORDER.*" response)        (list 'production-mode pid-sym 'make-to-order)
                       (re-matches #".*(?i)ENGINEER-TO-ORDER.*" response)    (list 'production-mode pid-sym 'engineer-to-order)
                       :else                                                 (list 'fails-query 'production-mode pid-sym))]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-activity-location-type ---------------------------------------------------------
(defoperator :!query-activity-location-type [obj]
  (let [query (str "Some work, for example factory work, must be performed in a specially designed facility. "
                   "Other work, like cutting down a tree, can only be performed at a location designated by the customer. "
                   "Are the processes you describe things that must be performed at your facility, or are they things that must be done at the customer's site? "
                   "Respond respectively with either the single term OUR-FACILITY or CUSTOMER-SITE.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair {:tags [:!query-activity-location-type] #_#_:tries 2})
        db-action)))

(defaction :!query-activity-location-type [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)OUR-FACILITY.*" response)  (list 'has-production-facility pid-sym)
                       (re-matches #".*(?i)CUSTOMER-SITE.*" response) (list 'performed-at-customer-site pid-sym)
                       :else                                          (list 'fails-query 'facility-vs-site pid-sym))]
    (db/add-planning-state pid [new-fact])))

;;; ----- :!query-shop-type ---------------------------------------------------------
(defoperator :!query-shop-type [obj]
  (let [query (str "A FLOW-SHOP is a production system designed so that all jobs follows the same sequence of steps through production resources. "
                   "A JOB-SHOP is a production system where each job might follow its own route, depending on its unique requirements. "
                   "Is the process you described more like a flow-shop or a job-shop? "
                   "Respond respectively with either the single term FLOW-SHOP or JOB-SHOP.")]
    (-> obj
        (assoc :agent-query query)
        (chat-pair {:tags [:!query-shop-type] #_#_:tries 2})
        db-action)))

(defaction :!query-shop-type [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)FLOW-SHOP.*" response)  (list 'flow-shop pid-sym)
                       (re-matches #".*(?i)JOB-SHOP.*"  response)  (list 'job-shop pid-sym)
                       :else                                       (list 'fails-query 'flow-vs-job pid-sym))]
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
        (chat-pair {:tags [:query-process-steps]}))))

(defaction :!query-process-steps [{:keys [pid response client-id] :as obj}]
  ;; Nothing to do here but update state from a-list.
  (let [more-state (inv/analyze-process-steps-response obj)]
    (log/info "---------------------- !query-process-steps: more-state = " more-state)
    (db/add-planning-state pid more-state)))

;;; ----- :!query-process-durs -----------------------------------------------------------
(defoperator :!query-process-durs [{:keys [state] :as obj}]
  (log/info "---------------------- !query-process-durs: state = "state)
  (let [agent-query (str "I suppose processing times for each of the steps you just mentioned might vary from product to product. "
                         "But generally speaking, how long does each step take? "
                         "Please produce a list just like the one you did for process steps, one process per line, but append to "
                         "each line the typical processing time in parentheses.")]
    (-> obj
        (assoc :agent-query agent-query)
        (chat-pair {:tags [:!query-process-durs]})
        db-action)))

(defaction :!query-process-durs [{:keys [client-id response pid] :as obj}]
  (log/info "!query-process-durs (action): response =" response "obj =" obj)
  (let [more-state (inv/analyze-process-durs-response obj)
        new-code (mzn/minimal-mzn-for-process pid)
        minizinc-enum-announce
        (str "Okay, we now know enough to get started on a MiniZinc solution. "
             "In the code pane (upper right of the app) we created a simplistic scheduling system."
             "It only illustrates the idea of running one job through each of the tasks you mentioned"
             "(excepting any tasks that weren't part of making the product, those we'll deal with later.")]
    (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
    (db/put-code pid new-code)
    ;; ToDo: This should really be just for humans. Maybe use the DB's message tags to decide that. (or to decide what to send).
    (db/add-msg pid :system minizinc-enum-announce [:info-to-user :minizinc])
    (ws/refresh-client client-id pid :process)
    (db/add-planning-state pid more-state)))
