(ns user
  "For REPL-based start/stop of the server.
   This file isn't used in cljs and is a problem for shadow-cljs without the
   :clj compiler directives."
  (:require
   [clojure.pprint]
   [clojure.string]
   [clojure.spec.alpha :as s]
   [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
   [datahike.api :as d]
   [develop.repl :refer [ns-setup! undo-ns-setup!]] ; for use at REPL.
   [develop.dutil]
   [expound.alpha :as expound]
   [mount.core :as mount]
   [lambdaisland.classpath.watch-deps :as watch-deps] ; hot loading for deps.

   [scheduling-tbd.mock :as mock]
   [scheduling-tbd.sutil :as sutil]

   [scheduling-tbd.core :refer [server]] ; for mount.
   [scheduling-tbd.db :as db] ; for run-demo!
   [scheduling-tbd.iviewr.interviewers :as inv]
   [scheduling-tbd.llm :as llm :refer [llm-tools]] ; Because of deep synchronization problems when this is from mount.
   [scheduling-tbd.surrogate :as sur]
   [scheduling-tbd.web.handler] ; for mount, defines rm.server.config/config, and router stuff.
   [scheduling-tbd.web.websockets :as ws] ; for run-demo!.
   [taoensso.telemere :as tel :refer [log!]]))

[server llm-tools ns-setup! undo-ns-setup!] ; for mount

;;; If you get stuck do: (clojure.tools.namespace.repl/refresh)

;; uncomment to enable hot loading for deps
(watch-deps/start! {:aliases [:dev :test]})

(alter-var-root #'s/*explain-out* (constantly expound/printer))
(add-tap (bound-fn* clojure.pprint/pprint))
(set-refresh-dirs "src/server/scheduling_tbd" #_"test/scheduling_tbd") ; Put here as many as you need. test messes with ns-setup!
(s/check-asserts true) ; Error on s/assert, run s/valid? rather than just returning the argument.
(tel/call-on-shutdown! tel/stop-handlers!)

(defn ^:admin start
  "Start the web server"
  []
  (try
    (let [res (mount/start)
          info (str "   " (clojure.string/join ",\n    " (:started res)))]
      (log! :info (str "started:\n" info)))
    (catch Exception e
      (log! :error (str "start might fail with a UnresolvedAddressException if the internet connection is bad.\n" e)))))

(defn stop
  "Stop the web server"
  []
  (mount/stop))

(defn ^:admin restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))

(defn ^:diag run-demo!
  ([] (run-demo! :sur-craft-beer))
  ([orig-pid]
   (let [client-id (or (ws/recent-client!) :console) ; Use :console if no client connected
         pid (sutil/shadow-pid orig-pid)
         cfg (sutil/db-cfg-map {:type :project :id pid :in-mem? true})]
     (when (d/database-exists? cfg) (mock/destroy-shadow-db! pid))
     (ws/send-to-client {:dispatch-key :load-proj :client-id client-id :promise? false
                         :new-proj-map {:project/name "Craft Beer (s)" :project/id pid}}) ; <============== Won't be seen anyway!
     (Thread/sleep 1000)
     (mock/with-mock-project orig-pid
       (inv/resume-conversation {:client-id client-id :pid orig-pid :cid :process})))))

