(ns scheduling-tbd.web.routes.websockets
  "Set up a websockets and message routing for async communication with the client using ring.websocket.async."
  (:require
   [clojure.core.async   :as async :refer [<! >! go]]
   [clojure.edn          :as edn]
   [clojure.walk         :as walk  :refer [keywordize-keys]]
   [mount.core :as mount :refer [defstate]]
   [ring.websocket.async :as wsa]
   [scheduling-tbd.util  :refer [now]]
   [taoensso.timbre      :as log]))

;;; Though this system is only exploratory code that will probably run single-user in a Docker instance,
;;; I'm writing this mostly as though the server handles multiple clients.
;;; I think it is just a little less error-prone this way while doing development.

(def ^:diag diag (atom {}))
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
  (when (contains? @socket-channels id)
    (let [{:keys [in out err]} (get @socket-channels id)]
      (log/info "Closing websocket channels for " id (now))
      (async/close! in)
      (async/close! out)
      (async/close! err)
      (swap! socket-channels #(dissoc % id)))))

(declare ws-send-client)

(defn ping-diag
  "Create a ping confirmation for use in middle of a round-trip."
  [{:keys [_client-id _ping-id]}]
  ;(log/info "Ping" ping-id "from" client-id)
  {:dispatch-key :ping-confirm})

(def dispatch-table
  "A map from keyword keys (typically values of :dispatch-key) to functions for websockets."
  {:ping ping-diag})

(defn dispatch [{:keys [dispatch-key] :as msg}]
  (when-not (= :ping dispatch-key) (log/info "Received msg:" msg))
  (if (contains? dispatch-table dispatch-key)
    ((get dispatch-table dispatch-key) msg)
    (log/error "No dispatch function for " msg)))

(defn establish-websocket-handler [request]
  (if-let [id (-> request :query-params keywordize-keys :client-id)]
    (do (close-ws-channels id)
        (let [{:keys [in out err]} (make-ws-channels id)]
          (log/info "Starting websocket handler for " id (now))
          (go ; This was wrapped in a try with (finally (close-ws-channels id)) but closing wasn't working out.
                (loop []
                  (when-let [msg (<! in)]
                    (>! out (-> msg edn/read-string dispatch str)) ; For round-trip messages from client.
                    (recur))))
          {:ring.websocket/listener (wsa/websocket-listener in out err)}))
    (log/error "Websocket client did not provide id.")))

(defn msg4chat
  [msg-text]
  {:dispatch-key :tbd-says
   :msg msg-text
   :timestamp (now)})

(defn ^:diag ws-send-client
  "Send the argument message to the specified client or current-client-id.
   This is only used for server-initiated interactions.
   Example usage: (ws-send-client \"Hello, world!\")."
  ([msg-text] (if-let [client-id @current-client-id]
                (ws-send-client msg-text client-id)
                (log/error "client-id not set in ws-send-client.")))
  ([msg-text client-id]
   (log/info "Sending msg = " msg-text)
   (if-let [out (->> client-id (get @socket-channels) :out)]
     (go (>! out (-> msg-text msg4chat str)))
     (log/error "Could not find out async channel for client" client-id))
   true))

;;;------------------- Starting and stopping ---------------
(defn wsock-start []
  :started!)

(defn wsock-stop []
  (doseq [id (keys @socket-channels)]
    (close-ws-channels id))
  [:closed-sockets])

(defstate wsock
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (wsock-start)
  :stop (wsock-stop))
