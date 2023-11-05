(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets, server side. See https://medium.com/pragmatic-programmers/multi-user-with-websockets-839f1459fe81"
  (:require
   [clojure.core.async :as a :refer [<! >!]]
   [ring.websocket.async :as wsa]
   [clojure.edn        :as edn]
   ;;[org.httpkit.server :as http-kit]
   [taoensso.timbre    :as log]))

(def diag (atom []))

(defn echo-websocket-handler [request]
  (log/info "**************I got a request!**************")
  (wsa/go-websocket [in out err]
    (loop []
      (when-let [mesg (<! in)]
        (swap! diag conj mesg)
        (>! out "this-instead")
        (recur)))))
