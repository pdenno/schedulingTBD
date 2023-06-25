(ns stbd-app.util
  (:require
   [applied-science.js-interop :as j]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

;;; ToDo: Is there a react way? It looks like react doesn't have this notion.
(def root "The application's root 'Symbol(react.element)' element" (atom nil))

#_(defn fn? [arg] ; I didn't see this in the docs!
  (= "Function" (j/get-in arg [:constructor :name])))

(def component-refs
  "Some components instances are named and their refs stored here."
  (atom  {}))

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
         (assoc :min-level [[#{"STBD-DEFAULT" "scheduling-tbd.*" "stdb-app.*" "user"} min-level]
                            [#{"datahike.*"} :error]
                            [#{"*"} :error]])))
     (log/error "Invalid timbre reporting level:" min-level)))

(defn default-min-log-level
  "Get the value of 'RM-DEFAULT' in (:min-level log/*config*), it designates
   the logging level for namespaces of scheduling-tbd stdb-app, and user."
  []
  (->> log/*config* :min-level (some #(when (contains? (first %) "STDB-DEFAULT") (second %)))))
