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
;;; in Step (1) ws/send-msg is used for humans, whereas llm/query-on-thread is used for surrogates.
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

(s/def ::response-ctx (s/keys :req-un [::response ::question-type ::cid]))
(s/def ::response string?)
(s/def ::question-type keyword?)
(s/def ::cid #(#{:process :data :resources :optimality} %))

(defn analyze-response--dispatch
  "Parameters to analyze-response is a object with at least a :plan-step in it and a response from operator-meth (user response)."
  [{:keys [response question-type cid] :as ctx}]
  (s/assert ::response-ctx ctx)
  (log! :debug (str "analyze-response-dispatch: ctx = " ctx "response = " response))
  (if (and question-type cid)
    (keyword (name cid) (name question-type))
    (do
      (reset! diag ctx)
      (log! :error (str "analyze-response-dispatch: No dispatch for question-type. ctx = " ctx))
      (throw (ex-info "No method to analyze response: " {:ctx ctx})))))

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
