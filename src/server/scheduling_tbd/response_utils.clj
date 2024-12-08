(ns scheduling-tbd.response-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.spec.alpha            :as s]
   [clojure.core.unify            :as uni]
   [clojure.pprint                :refer [cl-format]]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :refer [log!]]))

;;; Program behavior differs in places depending on whether the agent is human or surrogate. Most obviously,
;;; in Step (1) ws/send-msg is used for humans, whereas adb/query-on-thread is used for surrogates.
;;; For the most part, we look at the state vector and use the function (surogate? state) to vary the behavior.

(def ^:diag diag (atom nil))

(defmacro defanalyze
  "Macro to wrap methods for updating the project's database for effects from running an operation.
   Returned value is not meaningful."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map] & body)}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  `(defmethod analyze-response-meth ~tag [~arg-map]
     (log! :debug (cl-format nil "==> ~A (act)" ~tag))
     (let [res# (do ~@body)]
       (if (seq? res#) (doall res#) res#)
       (do (log! :debug (cl-format nil "<-- ~A (act) returns ~S" ~tag res#))
           res#))))

;;; Needs work: I get scheduling_tbd/web/websockets.clj:289 - Could not find out async channel for client
#_(defmacro defanalyze
  "Macro to wrap methods for updating the project's database for effects from running an operation.
   Returned value is not meaningful."
  {:clj-kondo/lint-as 'clojure.core/defmacro
   :arglists '(tag [arg-map] & body)}
  [tag [arg-map] & body]  ; ToDo: Currently to use more-args, the parameter list needs _tag before the useful one.
  (let [as-var# (:as arg-map)]
    `(defmethod analyze-response-meth ~tag [~arg-map]
       (try
         (do
           (when-let [client-id# (:client-id ~as-var#)]
             (ws/send-to-chat {:dispatch-key :interviewer-busy? :value true :client-id client-id#}))
           (log! :debug (cl-format nil "==> ~A (act)" ~tag))
           (let [res# (do ~@body)]
             (if (seq? res#) (doall res#) res#)
             (log! :debug (cl-format nil "<-- ~A (act) returns ~S" ~tag res#))
             res#))
         (finally
           (when-let [client-id# (:client-id ~as-var#)]
             (ws/send-to-chat {:dispatch-key :interviewer-busy? :value false :client-id client-id#})))))))


(s/def ::dispatch-args (s/keys :req-un [::question-type]))
(s/def ::question-type (s/and keyword?
                              #(#{"process" "data" "resources" "optimality"} (namespace %))))

(defn analyze-response--dispatch
  "Parameters to analyze-response is a object with at least a :plan-step in it and a response from operator-meth (user response)."
  [{:keys [question-type cid] :as args}]
  (reset! diag args)
  (s/assert ::dispatch-args args)
  (if (and question-type cid)
    (keyword (name cid) (name question-type))
    (do
      (log! :error (str "analyze-response-dispatch: No dispatch for question-type. ctx = " args))
      (throw (ex-info "No method to analyze response: " {:args args})))))

(defmulti analyze-response-meth #'analyze-response--dispatch)

(defn find-claim
  "Unify the fact (which need not be ground) to the fact-list"
  [fact fact-list]
  (some #(when (uni/unify fact %) %) fact-list))

(defn refresh-client
  "Send a message to the client to reload the conversation.
  Typically done with surrogate or starting a new project."
  [client-id pid cid]
  (assert (string? client-id))
  (ws/send-to-chat {:promise? false
                    :client-id client-id
                    :dispatch-key :update-conversation-text
                    :pname (db/get-project-name pid)
                    :pid pid
                    :cid cid}))
