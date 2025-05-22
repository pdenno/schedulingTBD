(ns user
  "For REPL-based start/stop of the server.
   This file isn't used in cljs and is a problem for shadow-cljs without the
   :clj compiler directives."
  (:require
   [clojure.pprint]
   [clojure.string]
   [clojure.spec.alpha :as s]
   [mount.core :as mount]
   [scheduling-tbd.core :refer [server]]                                ; for mount.
   [scheduling-tbd.interviewing.domain.process-analysis :as pan]        ; for mount.
   [scheduling-tbd.interviewing.domain.resources-analysis :as ru]       ; for mount.
   [scheduling-tbd.llm  :as llm]                                        ; Because of deep synchronization problems when this is from mount.
   [scheduling-tbd.interviewing.interviewers]                           ; for mount
   [scheduling-tbd.web.handler]                                         ; for mount, defines rm.server.config/config, and router stuff.
   [taoensso.telemere :as tel :refer [log!]]))

;;; If you get stuck do: (clojure.tools.namespace.repl/refresh)
(s/check-asserts true) ; Error on s/assert, run s/valid? rather than just returning the argument.
(tel/call-on-shutdown! tel/stop-handlers!)

(defn start
  "Start the web server"
  []
  (let [res (mount/start)
        info (str "   " (clojure.string/join ",\n    " (:started res)))]
    (log! :info (str "started:\n" info))))

(defn stop
  "Stop the web server"
  []
  (mount/stop))
