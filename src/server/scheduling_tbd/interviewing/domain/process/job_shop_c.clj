(ns scheduling-tbd.interviewing.domain.process.job-shop-c
  "(1) Define the an example annotated data structure (EADS) to provide to the interviewer for a job-shop scheduling problem.
       As the case is with job-shop problems, this structure defines work to be performed in typical job.
   (2) Define well-formedness constraints for this structure. These can also be used to check the structures produced by the interviewer.

   Note: We don't load this code at system startup. When you compile it, it writes the EADS to resources/EADS/job-shop-c.txt"
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))

(s/def :job-shop-c/EADS-message (s/keys :req-un [::message-type ::interview-objective ::interviewer-agent ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent #(= % :process))

(s/def ::comment string?) ; About annotations

(s/def ::EADS (s/keys :req-un [::EADS-id ::classifiable-jobs?]))
(s/def ::EADS-id #(= % :process/job-shop-c))
(s/def ::classifiable-jobs? (s/or :normal :classifiable-jobs?/val :annotated ::annotated-classifiable-jobs?))
(s/def :classifiable-jobs?/val boolean?)
(s/def ::annotated-classifiable-jobs? (s/keys :req-un [:classifiable-jobs?/val ::comment]))

;;; (s/valid? ::fshop/EADS (:EADS fshop/job-shop-c))
(def job-shop-c
  "A pprinted (JSON?) version of this is what we'll provide to the interviewer at the start of Phase 2 of a job-shop-c problem."
  {:message-type :EADS-INSTRUCTIONS
   :interviewer-agent :process
   :interview-objective (str "These EADS-INSTRUCTIONS assumes the interviewees' production processes are organized as a job shop.\n"
                             "We have two separate approaches to model job shop scheduling problems: in one of these, the jobs are classified to match a small (less than a dozen or so)\n"
                             "different process plans; in the other, a process plans will need to be specified for each job.\n"
                             "The purpose of these EADS-INSTRUCTIONS is only to determine which of these two subclasses of job shop models should be pursued.\n"
                             "Once this is determined, the orchestrator will likely then choose either 'process/job-shop--classified' or 'process/job-shop--unique' corresponding\n"
                             "respectively to the two separate approaches just described.")
   :EADS
   {:EADS-id :process/job-shop-c
    :classifiable-jobs? {:val true,
                         :comment "This property is true only in the case that it seems reasonable to classify jobs to corresponding to a small collection (a dozen or so) process plans."}}})

(if (s/valid? :job-shop-c/EADS-message job-shop-c)
  (let [db-obj {:EADS/id :process/job-shop-c
                :EADS/cid :process
                :EADS/specs #:spec{:full :job-shop-c/EADS-message}
                :EADS/msg-str (str job-shop-c)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]})
    ;; Write the EADS JSON to resources/EADS/process so it can be placed in ork's vector store.
    (->> job-shop-c clj2json-pretty (spit "resources/EADS/process/job-shop-c.json")))
  (throw (ex-info "Invalid EADS message (job-shop-c)." {})))
