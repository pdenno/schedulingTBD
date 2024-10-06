(ns scheduling-tbd.interviewer-test
  (:require
   [scheduling-tbd.db           :as db]
   [scheduling-tbd.interviewer  :as inv]
   [scheduling-tbd.llm          :as llm]
   [scheduling-tbd.sutil        :as sutil :refer [connect-atm default-llm-provider]]
   [taoensso.timbre :as log :refer [debug]]))

;;; THIS is the namespace I am hanging out in recently.

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'pinv   'scheduling-tbd.domain.process.p-interview)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'plan   'scheduling-tbd.planner)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'openai 'wkok.openai-clojure.api))

(defn tryme0 []
  (inv/create-interviewer (slurp "data/instructions/interviewer-process.txt")))


(defn tryme []
  (let [{:keys [aid tid]} (db/get-agent :process-interview-agent)]
    (letfn [(tell-agent [text] (log/info (llm/query-on-thread {:aid aid :tid tid :role "user" :query-text text})))]
      (tell-agent "{'command' : 'NEW-INTERVIEW'}")
      (tell-agent "{'command' : 'ANALYSIS-CONCLUDES', 'conclusions' : '1) They remanufacture plate glass. 2) You are talking to surrogate humans (machine agents).'}")
      (tell-agent "{'command' : 'NEXT-QUESTION'}")
      (tell-agent "{'command' : 'HUMAN-RESPONDS',
                    'response' :
'One of our most significant scheduling problems involves coordinating the production schedule with the supply of raw materials.
 Fluctuations in raw material deliveries can disrupt planned production runs, leading to delays and inefficiencies.
 Additionally, we need to balance these inconsistencies with varying demand from customers, ensuring that we meet order deadlines without overproducing and holding excess inventory.'}")
      (tell-agent "{'command' : 'NEXT-QUESTION'}")
      ;; Here I'm guessing, of course.
      (tell-agent "{'command' : 'HUMAN-RESPONDS',
'Our production process for plate glass involves several key stages.
 It starts with raw material preparation, where materials like silica sand, soda ash, and limestone are accurately measured and mixed.
 This mixture is then fed into a furnace and melted at very high temperatures to form molten glass.
 The molten glass is then formed into sheets using the float glass process, where it is floated on a bed of molten tin.
 After forming, the glass is slowly cooled to prevent stress in a process called annealing.
 Finally, the glass is cut to size, inspected for quality, and prepared for shipment.'")
      (tell-agent "{'command' : 'NEXT-QUESTION'}"))))


;;; Remember to db/backup-system-db once you get things straight.
(defn new-interviewer
  "Use this whenever you need to adjust the instructions."
  []
  (let [{:keys [id instruction-path response-path]}
        {:id :process-interview-agent
         :instruction-path     "data/instructions/interviewer-process.txt"
         :response-format-path "data/instructions/interviewer-response-format.edn"}]
        (cond-> {}
          true (assoc :id (-> id name (str "-" "openai") keyword))
          true (assoc :base-type id)
          true (assoc :llm-provider :openai)
          true (assoc :instructions (slurp instruction-path))
          #_#_response-format-path (assoc :response-format (-> response-format-path slurp edn/read-string))
          true db/add-agent!)))
