(ns develop.dutil
  "Tools for repl-based exploration of SchedulingTBD code"
  (:require
   [ajax.core :refer [GET POST]]
   [clojure.pprint :refer [pprint]]
   [promesa.core   :as p]))

(defn clean-form
  "Replace some namespaces with aliases"
  [form]
  (let [ns-alia {"scheduling-tbd.sutil"         "sutil"
                 "scheduling-tbd.shop"          "shop"
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

(defmacro ajax-test
  "Test an HTTP request, returning the response.
   Example usage:
       (require '[develop.dutil :as devl])
       (devl/ajax-test \"/api/get-conversation\" {:client-id \"8d0e3fdc-b4bf-446e-b4bf-0568aee96af0\"})."
  [uri params & {:keys [method] :or {method 'ajax.core/GET}}]
  `(let [prom# (promesa.core/deferred)
         req-data# {:params ~params
                    :handler (fn [resp#] (promesa.core/resolve! prom# resp#))
                    :error-handler (fn [{:keys [status# status-text#]}]
                                     (promesa.core/reject! prom# (ex-info ~(str "CLJS-AJAX error on " uri)
                                                                          {:status status# :status-text status-text#})))
                    :timeout 2000}]
     (~method ~(str "http://localhost:3300" uri) req-data#)
     (promesa.core/await prom# 2500)))
