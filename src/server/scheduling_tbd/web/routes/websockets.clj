(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:require
   [clojure.core.async   :as async :refer [<! >! go]]
   [clojure.walk         :as walk  :refer [keywordize-keys]]
   [ring.websocket.async :as wsa]
   [clojure.edn          :as edn]
   [taoensso.timbre      :as log]))

;;; Though this system is only exploratory code that will probably run single-user in a Docker instance,
;;; I'm writing this mostly as though the server handles multiple clients.
;;; I think it is just a little less error-prone this way while doing development.

(defn now [] (new java.util.Date))
(def diag (atom {}))
(def socket-channels "Indexed by a unique ID provided by the client." (atom {}))
(def current-client-id "UUID identifying client. Text above notwithstanding, this is a single-user idea." (atom nil))

(defn make-ws-channels
  "Create channels and store them keyed by the unique ID provided by the client."
  [id]
  (reset! current-client-id id)
  (let [chans {:in  (async/chan)
               :out (async/chan)
               :err (async/chan)}]
    (swap! socket-channels  #(assoc % id chans))
    chans))

(defn close-ws-channels [id]
  (let [{:keys [in out err]} (make-ws-channels id)]
    (log/info "Closing websocket channels for " id)
    (async/close! in)
    (async/close! out)
    (async/close! err)
    (swap! socket-channels #(dissoc % id))))

(defn ping-diag
  [{:keys [client-id]}]
  (log/info "Ping from" client-id)
  :ping-ok)

(def dispatch-table
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets."
  {:ping ping-diag})

(defn dispatch [{:keys [dispatch-key] :as msg}]
  (if (contains? dispatch-table dispatch-key)
    ((get dispatch-table dispatch-key) msg)
    (log/error "No dispatch function for " msg)))

(defn establish-websocket-handler [request]
  (if-let [id (-> request :query-params keywordize-keys :id)]
    (let [{:keys [in out err]} (make-ws-channels id)]
      (log/info "Starting websocket handler for " id)
      (go (try
            (loop []
              (when-let [msg (<! in)]
                (>! out (-> msg edn/read-string dispatch str))
                (recur)))
            (finally (close-ws-channels id))))
      {:ring.websocket/listener (wsa/websocket-listener in out err)})
    (log/error "Websocket client did not provide id.")))

(defn ws-send-client
  "Send the argument message to the specified client or current-client-id.
   Example usage: (ws-send-client {:tbd-says \"Hello, world!\"})."
  [{:keys [msg client-id]}]
  (let [client-id (or client-id @current-client-id)
        out (->> client-id (get @socket-channels) :out)]
    (if out
      (go (>! out (str msg)))
      (log/error "Could not find out async channel for client" client-id))))
