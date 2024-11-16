(ns scheduling-tbd.util
  "Do lowest level configuration (logging, etc.) used by both the server and app."
  (:require
   [clojure.string                  :as str]
   [clojure.pprint                  :refer [pprint]]
   #?@(:clj [[datahike.api          :as d]
             [datahike.pull-api     :as dp]
             [jansi-clj.core           :refer [yellow cyan bold]]
             [taoensso.telemere.tools-logging :as tel-log]])
   [mount.core                      :as mount :refer [defstate]]
   [taoensso.telemere               :as tel :refer [log!]]
   [taoensso.timbre                 :as timbre])) ; To stop pesky datahike :debug messages.

(def diag (atom nil))

#?(:clj
(defn custom-console-output-fn
  "I don't want to see hostname and time, etc. in console logging."
  [signal]
  (reset! diag signal)
  (let [{:keys [kind level location msg_]} signal
        file (:file location)
        file (when (string? file)
               (let [[_ stbd-file] (re-matches #"^.*(scheduling_tbd.*)$" file)]
                 (or stbd-file file)))
        line (:line location)
        msg (if (string? msg_) msg_ (deref msg_))
        heading (-> (str "\n" (name kind) "/" (name level) " ") str/upper-case)]
    (cond (= :warn level)                    (bold (cyan heading) (yellow file ":" line " - " msg))
          :else                              (str  (bold (cyan heading))  file ":" line " - " msg)))))

#?(:cljs
(defn custom-console-output-fn
  "Like clj one but without colors."
  [sig]
  (let [{:keys [kind level location line msg_]} sig
        [_ file] (re-matches #"^.*(schedulingTBD.*)$" (:file location))
        msg (if (string? msg_) msg_ (deref msg_))
        heading (-> (str "\n" (name kind) "/" (name level) " ") str/upper-case)]
    (str heading file ":" line " - " msg))))

(defn config-log!
  "Configure Telemere: set reporting levels and specify a custom :output-fn."
  []
  (let [output-fn custom-console-output-fn]
    (tel/add-handler! :default/console (tel/handler:console {:output-fn output-fn}))
    ;; (tel/set-min-level! :log "datahike.*" :error)             ; ToDo: make it work!
    ;;(tel-timbre/set-ns-min-level! "datahike.connector" :error) ; ToDo: make it work! Can I use "datahike.*" ?
    ;; https://github.com/taoensso/telemere/wiki/3-Config
    #?@(:clj ((tel-log/tools-logging->telemere!)  ;; Send tools.logging through telemere. Check this with (tel/check-interop)
              (tel/streams->telemere!)           ;; likewise for *out* and *err* but "Note that Clojure's *out*, *err* are not necessarily automatically affected."
              (tel/event! ::config-log {:level :info :msg (str "Logging configured:\n" (with-out-str (pprint (tel/check-interop))))}))
        :cljs ((log! :info "Logging configured.")))
    ;; The following is needed because of datahike; no timbre-logging->telemere!
    (timbre/set-config! (assoc timbre/*config* :min-level [[#{"datahike.*"} :error]
                                                           [#{"konserve.*"} :error]]))))

(defn ^:diag unconfig-log!
  "Set :default/console back to its default handler. Typically done at REPL."
  []
  (tel/remove-handler! :default/console)
  (tel/add-handler!    :default/console (tel/handler:console)))

(def max-duration
  "This is used in places where doing set-clock might not make sense."
  30000)

(def timeout-info "Used for calls to cljs-ajax and progress bar."
  (atom {:valid? false :max-millis max-duration :start-time nil :timeout-at nil}))

(defn invalidate-timeout-info
  []
  (swap! timeout-info #(assoc % :valid? false)))

#?(:clj
   (defn now [] (new java.util.Date))
   :cljs
   (defn now [] (.now js/Date) #_(.getTime (js/Date.))))

(defn start-clock
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
