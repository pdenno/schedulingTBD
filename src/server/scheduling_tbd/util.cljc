(ns scheduling-tbd.util
  "Do lowest level configuration (logging, etc.)."
  (:require
   #?(:clj [datahike.pull-api            :as dp])
   [mount.core :as mount :refer [defstate]]
   [taoensso.timbre :as log]))

;;; ToDo: The problem with output to log/debug might have to do with *err* not defined in cljs.
(defn custom-output-fn
  " - I don't want :hostname_ and :timestamp_ in the log output preface text.."
  ([data] (custom-output-fn nil data))
  ([opts data]
   (taoensso.timbre/default-output-fn opts (dissoc data :hostname_ :timestamp_))))

(defn config-log
  "Configure Timbre: set reporting levels and specify a custom :output-fn."
  [min-level]
  (if (#{:trace :debug :info :warn :error :fatal :report} min-level)
    (log/set-config!
     (-> log/*config*
         (assoc :output-fn #'custom-output-fn)
         (assoc :min-level [[#{"STBD-DEFAULT" "scheduling-tbd.*" "stbd-app.*" "user"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"*"} :error]])))
     (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  "Get the value of 'RM-DEFAULT' in (:min-level log/*config*), it designates
   the logging level for namespaces of scheduling-tbd stbd-app, and user."
  []
  (->> log/*config* :min-level (some #(when (contains? (first %) "STBD-DEFAULT") (second %)))))

;;; ToDo:
;;;  - cljs complains about not finding x/element-nss, which I don't see in the  0.2.0-alpha8 source at all.
;;;    (Yet it does work in clj!) I suppose reading xml isn't something I need in cljs, but it would be
;;;    nice to know what is going on here.
;;; ToDo: Get some more types in here, and in implementation generally.
#?(:clj
(defn db-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean)))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
#?(:clj
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj)))))

;;; {:db/id 3779}
#?(:clj
(defn resolve-db-id
  "Return the form resolved, removing properties in filter-set,
   a set of db attribute keys, for example, #{:db/id}."
  ([form conn-atm] (resolve-db-id form conn-atm #{}))
  ([form conn-atm filter-set]
   (letfn [(resolve-aux [obj]
             (cond
               (db-ref? obj) (let [res (dp/pull @conn-atm '[*] (:db/id obj))]
                               (if (= res obj) nil (resolve-aux res)))
               (map? obj) (reduce-kv (fn [m k v] (if (filter-set k) m (assoc m k (resolve-aux v))))
                                     {}
                                     obj)
               (vector? obj)      (mapv resolve-aux obj)
               (set? obj)    (set (mapv resolve-aux obj))
               (coll? obj)        (map  resolve-aux obj)
               :else  obj))]
     (resolve-aux form)))))

(def max-duration
  "This is used in places where doing set-clock might not make sense."
  30000)

(def timeout-info "Used for calls to cljs-ajax and progress bar."
  (atom {:valid? false :max-millis max-duration :start-time nil :timeout-at nil}))

(defn invalidate-timeout-info
  []
  (swap! timeout-info #(assoc % :valid? false)))

(defn start-clock
  "Set the timeout-info object and return the argument."
  ([] (start-clock max-duration))
  ([max-millis]
   (swap! timeout-info
          #(let [now #?(:clj (inst-ms (java.util.Date.)) :cljs (.getTime (js/Date.)))]
             (assoc % :valid? true :max-millis max-millis :start-time now :timeout-at (+ now max-millis))))
   max-millis))

(defn init-util []
  (config-log :info))

(defstate util-state
  :start (init-util))
