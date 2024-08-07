(ns scheduling-tbd.core
  "top-most file for starting the server, sets mount state server and system atom."
  (:require
   [ajax.core :refer [GET]] ; for testing
   [clojure.edn     :as edn]
   [clojure.java.io :as io]
   [clojure.string]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.db   :refer [sys&proj-database-cfgs]] ; for mount
   [scheduling-tbd.how-made :refer [him-cfg]]            ; for mount
   [scheduling-tbd.planner :refer [planning]]            ; for mount
   [scheduling-tbd.surrogate :refer [surrogates]]        ; for mount
   [scheduling-tbd.util :refer [util-state]]             ; for mount
   [scheduling-tbd.web.handler :refer [app]]             ; for mount
   [scheduling-tbd.web.websockets :refer [wsock]]        ; for mount
   [ring.adapter.jetty :as jetty]
   [taoensso.timbre :as log])
  (:gen-class))

;; log uncaught exceptions in threads
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      (log/error {:what :uncaught-exception
                  :exception ex
                  :where (str "Uncaught exception on" (.getName thread))}))))

(defonce system (atom nil))

(defn stop-server [& {:keys [profile] :or {profile :dev}}]
  (.stop @system)
  (reset! system nil)
  (when (= profile :prod) (shutdown-agents)))

(defn ^:diag test-server [port]
  (try
    ;; Convert the Ring handler into a running web server.
    (GET (str "http://localhost:" port "/api/health")
         {:handler (fn [resp] (log/info "Response through server (GET):" resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (log/error "Server fails response through server: status = " status " status-text = " status-text)
                           (throw (ex-info "Server fails health test." {:status status :status-text status-text})))
          :timeout 1000})
    (catch Throwable t
      (log/error t (str "server failed to start on port: " port)))))

;;; There's a lot to learn here about the server abstraction; it is explained here: https://github.com/ring-clojure/ring/wiki
(defn start-server [& {:keys [profile] :or {profile :dev}}]
  (let [base-config (-> "system.edn" io/resource slurp edn/read-string profile)
        port (-> base-config :server/http :port)
        host (-> base-config :server/http :host)]
    (try (let [server (jetty/run-jetty #'scheduling-tbd.web.handler/app {:port port, :join? false})]
           (reset! system server)
           ;(test-server port)
           (log/info "Started server on port" port))
         (catch Throwable t
           (log/error t "Server failed to start on host " host " port " port ".")))))

(defn -main [& _]
  (let [res (mount/start)
        info (str "   " (clojure.string/join ",\n    " (:started res)))]
    (log/info "started:\n" info)))

;;; This is top-most state for starting the server; it happens last.
(defstate server
  :start (start-server)
  :stop (stop-server))
