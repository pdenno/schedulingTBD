(ns develop.repl
  "Tools for repl-based exploration of SchedulingTBD code"
  (:require
   [clojure.pprint :refer [pprint]]))

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
  (safe-alias 'ches   'cheshire.core)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'json   'jsonista.core)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'fshop  'scheduling-tbd.interviewing.domain.process.flow-shop)
  (safe-alias 'jshop  'scheduling-tbd.interviewing.domain.process.job-shop)
  (safe-alias 'sptype 'scheduling-tbd.interviewing.domain.process.scheduling-problem-type)
  (safe-alias 'pan    'scheduling-tbd.interviewing.domain.process.process-analysis)
  (safe-alias 'inv    'scheduling-tbd.interviewing.interviewers)
  (safe-alias 'ork    'scheduling-tbd.interviewing.ork)
  (safe-alias 'ru     'scheduling-tbd.interviewing.response-utils)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'or     'scheduling-tbd.orchestrator)
  (safe-alias 'ort    'scheduling-tbd.orchestrator-test)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'tel    'taoensso.telemere)
  (safe-alias 'openai 'wkok.openai-clojure.api))

(defn clean-form
  "Replace some namespaces with aliases"
  [form]
  (let [ns-alia {"scheduling-tbd.sutil"         "sutil"
                 "promesa.core"                 "p"
                 "clojure.spec.alpha"           "s"
                 "java.lang.Math"               "Math"}
        ns-alia (merge ns-alia (zipmap (vals ns-alia) (vals ns-alia)))] ; ToDo: Make it more general. (Maybe "java.lang" since j.l.Exception too.)
    (letfn [(ni [form]
              (let [m (meta form)]
                (cond (vector? form) (-> (->> form (map ni) doall vec) (with-meta m)),
                      (seq? form)    (-> (->> form (map ni) doall) (with-meta m)),
                      (map? form)    (-> (reduce-kv (fn [m k v] (assoc m k (ni v))) {} form) (with-meta m)),
                      (symbol? form) (-> (let [nsa (-> form namespace ns-alia)]
                                           (if-let [[_ s] (re-matches #"([a-zA-Z0-9\-]+)__.*" (name form))]
                                             (symbol nsa s)
                                             (->> form name (symbol nsa))))
                                         (with-meta m)),
                      :else form)))]
      (ni form))))

(defn nicer
  "Show macroexpand-1 pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form clean-form) #_(-> form macroexpand-1 clean-form) ; ToDo: problem with macroexpand-1 in cljs?
          pprint? pprint))

(defn nicer-
  "Show pretty-printed form sans package names.
   Argument is a quoted form"
  [form & {:keys [pprint?] :or {pprint? true}}]
        (cond-> (-> form clean-form)
          pprint? pprint))

(defn remove-meta
  "Remove metadata from an object and its substructure.
   Changes records to maps too."
  [obj]
  (cond (map? obj) (reduce-kv (fn [m k v] (assoc m k (remove-meta v))) {} obj)
        (vector? obj) (mapv remove-meta obj)
        (seq? obj) (map remove-meta obj)
        :else obj))

(defn nicer-sym
  "Forms coming back from bi/processRM have symbols prefixed by clojure.core
   and other namespaces. On the quoted form in testing, I'd rather not see this.
   This takes away those namespace prefixes."
  [form]
  (clean-form form))
