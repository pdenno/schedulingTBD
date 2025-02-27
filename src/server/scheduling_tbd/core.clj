(ns scheduling-tbd.core
  "top-most file for starting the server, sets mount state server and system atom."
  (:require
   [ajax.core :refer [GET]] ; for testing
   [clojure.edn     :as edn]
   [clojure.java.io :as io]
   [clojure.string]
   [mount.core :as mount :refer [defstate]]
   [ring.adapter.jetty :as jetty]
   [scheduling-tbd.db   :refer [sys&proj-database-cfgs]] ; for mount
   [scheduling-tbd.how-made :refer [him-cfg]]            ; for mount
   [scheduling-tbd.interviewing.interviewers :refer [iviewers]]       ; for mount
   [scheduling-tbd.surrogate :refer [surrogates]]        ; for mount
   [scheduling-tbd.web.handler :refer [app]]             ; for mount
   [scheduling-tbd.web.websockets :refer [wsock]]        ; for mount
   [taoensso.telemere  :refer [log!]])
  #_(:gen-class))

;;; Here are some naming conventions we try to use throughout the server and app code.
;;;   pid - a project id (keyword)
;;;   aid - an assistant id (string)
;;;   tid - a thread id (string)
;;;   cid - a conversation id, currently one of #{:process :data :resources :optimality}.

;; Log uncaught exceptions in threads.
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (log! :error (str {:what :uncaught-exception
                         :exception ex
                         :where (str "Uncaught exception on" (.getName thread))})))))

(defonce system (atom nil))

(defn stop-server [& {:keys [profile] :or {profile :dev}}]
  (.stop @system)
  (reset! system nil)
  (when (= profile :prod) (shutdown-agents)))

(defn ^:diag test-server [port]
  (try
    ;; Convert the Ring handler into a running web server.
    (GET (str "http://localhost:" port "/api/health")
         {:handler (fn [resp] (log! :info (str "Response through server (GET): " resp)))
          :error-handler (fn [{:keys [status status-text]}]
                           (log! :error (str "Server fails response through server: status = " status " status-text = " status-text))
                           (throw (ex-info "Server fails health test." {:status status :status-text status-text})))
          :timeout 1000})
    (catch Throwable _e
      (log! :error (str "server failed to start on port: " port)))))

;;; There's a lot to learn here about the server abstraction; it is explained here: https://github.com/ring-clojure/ring/wiki
(defn start-server [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp edn/read-string profile)
        port (-> base-config :server/http :port)
        host (-> base-config :server/http :host)]
    (try (let [server (jetty/run-jetty #'scheduling-tbd.web.handler/app {:port port, :join? false})]
           (reset! system server)
           ;(test-server port)
           (log! :info (str "Started server on port " port)))
         (catch Throwable _e
           (log! :error (str "Server failed to start on host " host " port " port ".")))))
  [:http-server])

;;; ToDo: Do something to stop telemere console handlers. https://github.com/taoensso/telemere/blob/master/examples.cljc
#_(defn -main [& _]
  (let [res (mount/start)
        info (str "   " (clojure.string/join ",\n    " (:started res)))]
    (log! :info (str "started:\n" info))))

;;; This is top-most state for starting the server; it happens last.
(defstate server
  :start (start-server)
  :stop (stop-server))

;;;==================================== Uncomment this if, at startup, the program stops without a stack trace. =============================
#_(import '(java.util Arrays))

#_(let [shutdown-hook (Thread.
                       #(do
                          (println "Shutdown hook called")
                          (doseq [[thread stack-trace] (.entrySet (Thread/getAllStackTraces))]
                            (println thread ":" (java.util.Arrays/toString stack-trace)))))]
    (.addShutdownHook (Runtime/getRuntime) shutdown-hook))
