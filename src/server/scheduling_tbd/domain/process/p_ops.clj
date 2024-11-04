(ns scheduling-tbd.domain.process.p-ops
  "Planning operators for the process interview"
  (:require
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.domain.process.p-interview :as pinv]
   [scheduling-tbd.minizinc                   :as mzn]
   [scheduling-tbd.op-utils                   :as ou :refer [db-action defaction make-human-project surrogate?]]
   [scheduling-tbd.sutil                      :as sutil :refer [find-fact]]
   [scheduling-tbd.web.websockets             :as ws]
   [taoensso.timbre                           :as log]))

;;; Place this in any file where you are developing the process interview so updates will be found.
#_(register-planning-domain :process  (-> "data/planning-domains/process-interview.edn" slurp edn/read-string))

(def ^:diag diag (atom nil))

;;; ----- :describe-challenge
;;; (op/db-action (assoc op/example :client-id (ws/recent-client!)))
(defaction :!describe-challenge [{:keys [state response client-id] :as _obj}]
  (log/info "*******db-action (!describe-challenge): response =" response "state =" state)
  (let [surrogate? (surrogate? state)
        analysis-state (pinv/analyze-intro-response response state)] ; return state props proj-id and proj-name if human, otherwise argument state.
    (when-not surrogate? (make-human-project analysis-state))
    ;;--------  Now human/surrogate can be treated nearly identically ---------
    (let [[_ pid]   (find-fact '(proj-id ?x) analysis-state)
          [_ pname] (find-fact '(proj-name ?x) analysis-state)
          pid (keyword pid)]
      (doseq [f analysis-state] (db/add-claim pid f))
      (db/add-msg {:pid pid
                   :from :system
                   :text (format "Great, we'll call your project %s." pname)
                   :tags [:!describe-challenge :informative]})
      (ws/refresh-client client-id pid :process))))

;;; Above :!describe-challenges may add-planning-state (cites-raw-material-challenge ?p). ; ToDo: Do this for Interviewers (probably in the above).
;;;
;;; ToDo: Could be several more "cites" in the intro paragraph. For example, consider this, from sur-plate-glass:
;;; "Our most significant scheduling problem revolves around coordinating the manufacturing process with the fluctuating availability of raw materials,
;;; particularly high-quality silica sand and soda ash, and accommodating the variable demand from our customers.
;;; We struggle with optimizing machine utilization time and reducing downtime, while also managing delivery schedules that are dependent
;;; on these unpredictable elements. Additionally, managing the workforce to align with these changes,
;;; ensuring we have enough skilled workers available when needed, adds another layer of complexity to our operations.",
;;;
;;; This might get into skills classification, delivery schedules, downtime, machine utilization.

;;; ----- :!describe-process -----------------------------------------------------------------
#_(defoperator :!remark-raw-material-challenge [{:keys [pid client-id] :as _obj}]
  (let [info (str "Though you've cited a challenge with inputs (raw material, workers, or other resources), "
                 "we'd like to put that aside for a minute and talk about the processes that make product.")]
    (db/add-msg pid :system info [:!remark-raw-material-challenge :informative])
    (ws/refresh-client client-id pid :process)))

;;; ----- :!query-product-or-service ---------------------------------------------------------
(defaction :!query-product-or-service [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)PRODUCT.*" response) (list 'provides-product pid-sym)
                       (re-matches #".*(?i)SERVICE.*" response) (list 'provides-service pid-sym)
                       :else                                    (list 'fails-query 'product-or-service? pid-sym))]
    (db/add-claim pid new-fact)))

;;; ----- :!query-production-mode ---------------------------------------------------------
(defaction :!query-production-mode [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)MAKE-TO-STOCK.*" response)        (list 'production-mode pid-sym 'make-to-stock)
                       (re-matches #".*(?i)MAKE-TO-ORDER.*" response)        (list 'production-mode pid-sym 'make-to-order)
                       (re-matches #".*(?i)ENGINEER-TO-ORDER.*" response)    (list 'production-mode pid-sym 'engineer-to-order)
                       :else                                                 (list 'fails-query 'production-mode pid-sym))]
    (db/add-claim pid new-fact)))

;;; ----- :!query-activity-location-type ---------------------------------------------------------
(defaction :!query-activity-location-type [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)OUR-FACILITY.*" response)  (list 'has-production-facility pid-sym)
                       (re-matches #".*(?i)CUSTOMER-SITE.*" response) (list 'performed-at-customer-site pid-sym)
                       :else                                          (list 'fails-query 'facility-vs-site pid-sym))]
    (db/add-claim pid new-fact)))

;;; ----- :!query-shop-type ---------------------------------------------------------
(defaction :!query-shop-type [{:keys [pid response] :as _obj}]
  (let [pid-sym (-> pid name symbol)
        new-fact (cond (re-matches #".*(?i)FLOW-SHOP.*" response)  (list 'flow-shop pid-sym)
                       (re-matches #".*(?i)JOB-SHOP.*"  response)  (list 'job-shop pid-sym)
                       :else                                       (list 'fails-query 'flow-vs-job pid-sym))]
    (db/add-claim pid new-fact)))

;;; ----- :!query-process-steps. This is for flow shops. ------------------------------------------------
(defaction :!query-process-steps [{:keys [pid response client-id] :as obj}]
  ;; Nothing to do here but update state from a-list.
  (let [more-state (pinv/analyze-process-steps-response obj)]
    (log/info "---------------------- !query-process-steps: more-state = " more-state)
    (doseq [f more-state] (db/add-claim pid f))))

;;; ----- :!query-process-durs -----------------------------------------------------------
(defaction :!query-process-durs [{:keys [client-id response pid] :as obj}]
  (log/info "!query-process-durs (action): response = \n" response)
  (let [more-state (pinv/analyze-process-durs-response! obj)]
    (if (-> (db/get-process pid :initial-unordered) :process/sub-processes not-empty)
      (let [new-code (mzn/minimal-mzn-for-process pid)
            study-the-code-msg
            (str "Okay, we now know enough to get started on a MiniZinc solution. "
                 "In the code pane (upper right of the app) we created a simplistic scheduling system."
                 "It only illustrates the idea of running one job through each of the tasks you mentioned"
                 "(excepting any tasks that weren't part of making the product, those we'll deal with later.")]
        (ws/send-to-chat {:client-id client-id :dispatch-key :update-code :text new-code})
        (db/put-code pid new-code)
        ;; ToDo: This should really be just for humans. Maybe use the DB's message tags to decide that. (or to decide what to send).
        (db/add-msg {:pid pid
                     :from :system
                     :text study-the-code-msg
                     :tags [:info-to-user :minizinc]})
        (ws/refresh-client client-id pid :process)
        (doseq [f more-state] (db/add-claim pid f)))
      (ws/send-to-chat
       {:client-id client-id
        :dispatch-key :tbd-says
        :text "There was a problem defining a process corresponding to what you said."}))))

(defaction :!query-process-ordering [{:keys [response] :as _obj}]
  (log/info "!query-process-ordering: response =" response))
