(ns scheduling-tbd.domain.data-analysis
    "Planning operators for the data interview"
  (:require
   [clojure.edn                               :as edn]
   [clojure.pprint                            :refer [pprint]]
   [clojure.spec.alpha                        :as s]
   [clojure.string                            :as str]
   [jsonista.core                             :as json]
   [scheduling-tbd.db                         :as db]
   [scheduling-tbd.llm                        :as llm :refer [query-llm]]
   [scheduling-tbd.minizinc                   :as mzn]
   [scheduling-tbd.response-utils             :as ru :refer [defanalyze find-claim make-human-project]]
   [scheduling-tbd.sutil                      :as sutil :refer [elide starting-new-project?]]
   [scheduling-tbd.web.websockets             :as ws]
   [taoensso.telemere                         :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(def warm-up-question
  (str "What kind of spreadsheets do you use to help with your production scheduling?"))

(defanalyze :data/warm-up [{:keys [response _client-id _pid] :as _ctx}]
  (log! :debug (str "*******analysis :data/warm-up, response = " response)))
