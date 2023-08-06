(ns scheduling-tbd.util
  "Do lowest level configuration (logging, etc.)."
  (:require
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
