(ns scheduling-tbd.interviewer
  "Functions and operators implementing an agent-based interviewer"
  (:require
   [clojure.string           :as str]
   [datahike.api             :as d]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre          :as log]))


(defn create-interviewer
  "create an interviewer agent"
  [instructions]
  (llm/make-assistant
   :instructions instructions
   ;; response_format might not be used, but OTOH, you can't specify :response-format (with a hyphen).
   :response_format {:type "json_schema",
                     :json_schema {:name "interview_response",
                                   :strict true,
                                   :schema {:type "object",
                                            :properties {:question {:type "string"},
                                                       :status  {:type "string"}}
                                            :required ["status" "question"],
                                            :additionalProperties false}}}))
