(ns scheduling-tbd.util
  "Do lowest level configuration (logging, etc.) used by both the server and app."
  (:require
   [bling.core                      :as bling :refer [bling print-bling]]  ; print-pling is used (clj)!
   [clojure.pprint                  :refer [pprint]]                       ; pprint is used (clj)!
   [clojure.string                  :as str]
   #?@(:clj [[taoensso.telemere.tools-logging :as tel-log]])
   [mount.core                      :as mount :refer [defstate]]
   [taoensso.telemere               :as tel :refer [log!]]                ; log is used!
   [taoensso.timbre                 :as timbre])) ; To stop pesky datahike :debug messages.

(def diag (atom nil))

#?(:clj
   (defn pr-bling [x] x)
   :cljs
   (defn pr-bling [x] (print-bling x)))

(defn custom-console-output-fn
  "I don't want to see hostname and time, etc. in console logging."
  ([] :can-be-a-no-op) ; for shutdown, at least.
  ([signal]
   (when-not (= (:kind signal) :agents)
     (let [{:keys [kind level location msg_]} signal
           file (:file location)
           file (when (string? file)
                  (let [[_ stbd-file] (re-matches #?(:clj #"^.*(scheduling_tbd.*)$" :cljs #"^.*(stbd_app.*)$") file)]
                    (or stbd-file file)))
           line (:line location)
           msg (if-let [s (not-empty (force msg_))] s "\"\"")
           heading (-> (str "\n" (name kind) "/" (name level) " ") str/upper-case)]
       (cond (= :error level)      (pr-bling (bling [:bold.red.white-bg heading] " " [:red    (str file ":" line " - " msg)]))
             (= :warn  level)      (pr-bling (bling [:bold.blue heading]         " " [:yellow (str file ":" line " - " msg)]))
             :else                 (pr-bling (bling [:bold.blue heading]         " " [:olive  (str file ":" line " - " msg)])))))))

(defn agents-log-output-fn
  "Output verbatim agent interactions to a log file."
  ([] :can-be-a-no-op)
  ([signal]
   (when (= (:kind signal) :agents)
     (let [{:keys [inst msg_]} signal
           msg (if-let [s (not-empty (force msg_))] s "\"\"")]
       (str inst ": " msg "\n")))))

(defn config-log!
  "Configure Telemere: set reporting levels and specify a custom :output-fn."
  []
  (tel/add-handler! :default/console (tel/handler:console {:output-fn custom-console-output-fn}))
  #?(:clj (tel/add-handler! :agent/log (tel/handler:file  {:output-fn agents-log-output-fn
                                                           :path "./logs/agents-log.txt"
                                                           :interval :daily})))
;;; Alternative to above; separate handlers; not working:
;;;  #?(:clj  (tel/add-handler! :default/console (tel/handler:console {:output-fn custom-console-output-fn})))
;;;  #?(:cljs (tel/add-handler! :cljs/repl       (tel/handler:console {:output-fn cljs-repl-output-fn})))
  ;; https://github.com/taoensso/telemere/wiki/3-Config
  #?@(:clj ((tel-log/tools-logging->telemere!)  ;; Send tools.logging through telemere. Check this with (tel/check-interop)
            (tel/streams->telemere!)           ;; likewise for *out* and *err* but "Note that Clojure's *out*, *err* are not necessarily automatically affected."
            (tel/event! ::config-log {:level :info :msg (str "Logging configured:\n" (with-out-str (pprint (tel/check-interop))))}))
      :cljs ((log! :info "Logging configured.")))
  ;; The following is needed because of datahike; no timbre-logging->telemere!
  (timbre/set-config! (assoc timbre/*config* :min-level [[#{"datahike.*"} :error]
                                                         [#{"konserve.*"} :error]])))

(defn ^:diag unconfig-log!
  "Set :default/console back to its default handler. Typically done at REPL."
  []
  (tel/remove-handler! :default/console)
  (tel/add-handler!    :default/console (tel/handler:console)))

#_(def max-duration
  "This is used in places where doing set-clock might not make sense."
  30000)

#_(def timeout-info "Used for calls to cljs-ajax and progress bar."
  (atom {:valid? false :max-millis max-duration :start-time nil :timeout-at nil}))

#_(defn invalidate-timeout-info
  []
  (swap! timeout-info #(assoc % :valid? false)))

#?(:clj
   (defn now [] (new java.util.Date))
   :cljs
   (defn now [] (.now js/Date) #_(.getTime (js/Date.))))

#_(defn start-clock
  "Set the timeout-info object and return the argument."
  ([] (start-clock max-duration))
  ([max-millis]
   (swap! timeout-info
          #(let [now (now)]
             (assoc % :valid? true :max-millis max-millis :start-time (now) :timeout-at (+ now max-millis))))
   max-millis))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; -------------- Starting and stopping ----------------------
(defn init-util []
  (config-log!))

(defstate util-state
  :start (init-util))
