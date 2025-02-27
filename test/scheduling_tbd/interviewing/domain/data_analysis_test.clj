(ns scheduling-tbd.domain.process-analysis-test
  "Currently these are more about exploring prompts than they are about test the code."
  (:require
   [clojure.core.unify                     :as uni]
   [clojure.test                           :refer [deftest is testing]]
   [clojure.spec.alpha                     :as s]
   [datahike.api                           :as d]
   [datahike.pull-api                      :as dp]
   [jsonista.core                          :as json]
   [scheduling-tbd.agent-db                :as adb]
   [scheduling-tbd.db                      :as db]
   [scheduling-tbd.interviewing.domain.process-analysis :as pan]
   [scheduling-tbd.interviewing.interviewers            :as inv :refer [tell-interviewer]]
   [scheduling-tbd.llm                     :as llm :refer [query-llm]]
   [scheduling-tbd.response-utils          :as ru]
   [scheduling-tbd.util                    :as util]
   [scheduling-tbd.sutil                   :as sutil]
   [scheduling-tbd.web.websockets          :as ws]
   [taoensso.telemere                      :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn ^:diag ns-start-over!
  "This one has been useful. If you get an error evaluate this ns, (the declaration above) run this and try again."
  []
  (map (partial ns-unalias *ns*) (keys (ns-aliases *ns*))))

(defn ^:diag remove-alias
  "This one has NOT been useful!"
  [al ns-sym]
  (swap! alias? (fn [val] (->> val (remove #(= % al)) set)))
  (ns-unalias (find-ns ns-sym) al))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

;;;I've found this function super useful as it doesn't just loop inside the REPL, but actually outputs to the web interface
(defn testing-loop-sur
  [pid cid]
  (let [client-id (ws/recent-client!)
        ctx (merge (inv/ctx-surrogate {:pid pid :cid cid}) {:client-id client-id})
        conversation (inv/q-and-a ctx)
        response (-> conversation last)]

    (doseq [msg conversation]
      (db/add-msg (merge {:pid pid :cid cid} msg)))
    (inv/analyze-response pid cid response)
    (do
      (inv/tell-interviewer {:message-type "INTERVIEWEES-RESPOND"
                             :response (:text response)} ctx)
      (ru/refresh-client client-id pid cid))))