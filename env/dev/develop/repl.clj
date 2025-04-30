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

(def alias-map
  {'ches   'cheshire.core
   'io     'clojure.java.io
   's      'clojure.spec.alpha
   'uni    'clojure.core.unify
   'edn    'clojure.edn
   'str    'clojure.string
   'd      'datahike.api
   'dp     'datahike.pull-api
   'mount  'mount.core
   'p      'promesa.core
   'px     'promesa.exec
   'adb    'scheduling-tbd.agent-db
   'core   'scheduling-tbd.core
   'db     'scheduling-tbd.db
   'how    'scheduling-tbd.how-made
   'llm    'scheduling-tbd.llm
   'llmt   'scheduling-tbd.llm-test
   'fshop  'scheduling-tbd.interviewing.domain.process.flow-shop
   'jshop  'scheduling-tbd.interviewing.domain.process.job-shop
   'sptype 'scheduling-tbd.interviewing.domain.process.scheduling-problem-type
   'pan    'scheduling-tbd.interviewing.domain.process.process-analysis
   'inv    'scheduling-tbd.interviewing.interviewers
   'ork    'scheduling-tbd.interviewing.ork
   'orkt   'scheduling-tbd.interviewing.ork_test
   'ru     'scheduling-tbd.interviewing.response-utils
   'mzn    'scheduling-tbd.minizinc
   'mznt   'scheduling-tbd.minizinc-test
   'ou     'scheduling-tbd.op-utils
   'opt    'scheduling-tbd.operators-test
   'or     'scheduling-tbd.orchestrator
   'ort    'scheduling-tbd.orchestrator-test
   'spec   'scheduling-tbd.specs
   'sutil  'scheduling-tbd.sutil
   'sur    'scheduling-tbd.surrogate
   'surt   'scheduling-tbd.surrogate-test
   'util   'scheduling-tbd.util
   'resp   'scheduling-tbd.web.controllers.respond
   'ws     'scheduling-tbd.web.websockets
   'tel    'taoensso.telemere
   'openai 'wkok.openai-clojure.api})


(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (doseq [[a nspace] alias-map]
    (safe-alias a nspace)))

(defn ^:diag ns-fix-setup!
  "Remove all the namespace aliases from the argument namespace. Then you can recompile it."
  [ns-sym]
  (when-let [tns (find-ns ns-sym)]
    (binding [*ns* tns]
      (doseq [a (keys alias-map)]
        (ns-unalias *ns* a)))))

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
