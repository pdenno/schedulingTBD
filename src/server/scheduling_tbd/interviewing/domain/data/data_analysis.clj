(ns scheduling-tbd.interviewing.domain.data.data-analysis
  "Planning operators for the data interview"
  (:require
   [clojure.spec.alpha                        :as s]
   [scheduling-tbd.llm                        :as llm]
   [scheduling-tbd.interviewing.response-utils             :as ru :refer [analyze-warm-up]]
   [taoensso.telemere                         :as tel :refer [log!]]
   [scheduling-tbd.agent-db                   :as adb]))

(def ^:diag diag (atom nil))

(def the-warm-up-type-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(defmethod analyze-warm-up :data [_tag response]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))

(s/def ::table-list vector?)
(s/def ::data-step-2-conclusion 
       (s/keys :opt-un [::production-bottlenecks ::maintenance-scheduling ::unscheduled-downtime
                 ::worker-availability ::skilled-worker-availability ::demand-variation
                 ::product-variation])) ;make some of these required?
(s/def ::data-path (s/keys :req-un [::path ::budget]))

(s/def ::production-bottlenecks (s/coll-of ::production-bottleneck))
(s/def ::production-bottleneck string?) ;make specific bottleneck list?
(s/def ::maintenance-scheduling number?)
(s/def ::unscheduled-downtime number?)
(s/def ::worker-availability number?)
(s/def ::skilled-worker-availability number?)

(s/def ::demand-variation keyword?)
(s/def ::product-variation keyword?)

;Can't get this to work
;(gen/sample (s/gen ::product-variation)) or (gen/sample (s/gen ::demand-variation))
;(defn possible-demand-variation [x] (#{:seasonal :constant} x))
;(s/def ::demand-variation possible-demand-variation)

;(defn possible-product-variation [x] {#{:none :internal :external :both} x})
;(s/def ::product-variation possible-product-variation)

(s/def ::budget number?) ;this is duplicated from interviewers.clj, not great
(s/def ::path string?) ;there's probably a way better way to check that this is a valid path

(defn upload-first-vector-store-to-agent 
  [file-id pid]
  (log! :info "Unimplemented")
  )

(defn add-additional-vector-store-to-agent
  [vector-store-id pid]
  (let [interviewer-agent (adb/ensure-agent! (-> (get @adb/agent-infos :data-interviewer-agent) (assoc :pid pid)))]
    (llm/add-vector-store-to-assistant vector-store-id pid)
    )
)

(defn add-spreadsheet-to-agent
  [pid data-path]
  (log! :info "Unimplemented")
  )

(defn analyze-response
  [pid response]
  (log! :info "Unimplemented"))

