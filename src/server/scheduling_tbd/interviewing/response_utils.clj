(ns scheduling-tbd.interviewing.response-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.spec.alpha            :as s]
   [clojure.datafy                :refer [datafy]]
   [clojure.core.unify            :as uni]
   [clojure.pprint                :refer [cl-format pprint]]
   [clojure.string                :as str]
   [jsonista.core                 :as json]
   [scheduling-tbd.agent-db       :as adb]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :refer [log!]]))

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

(s/def ::text-to-var (s/keys :req-un [::INPUT-TEXT ::CORRESPONDING-VAR]))
(s/def ::INPUT-TEXT string?)
(s/def ::CORRESPONDING-VAR string?)

(defn good-for-var?
  "Returns true if the argument text has any hope of making a good variable name."
  [text]
  (and (< (count text) 25)
       (not-any? #{\. \, \{ \} \( \) \* \[ \] \@ \# \% \^} (set text))))

(defn make-var-from-string
  [text]
  (-> text
      str/trim
      (str/replace #"\s+" "-")
      str/lower-case))

(defn text-to-var
  "Convert some text to variable name, return as a string.
   This uses a agent only in the case that the text is long or has some characters in it that wouldn't look good in a variable
   (regarding this, see function good-for-var?)
   Call the text-to-var agent on the argument text, returning a map containing keys:
   :INPUT-TEXT and :CORRESPONDING-VAR."
  [text]
  (if (good-for-var? text)
    (make-var-from-string text)
    (let [res (try (-> (adb/query-agent :text-to-var (format "{\"INPUT-TEXT\" : \"%s\"}" text))
                       json/read-value
                       (update-keys keyword))
                   (catch Exception e
                     (let [d-e (datafy e)]
                       (log! :warn (str "text-to-var failed\n "
                                        "\nmessage: " (-> d-e :via first :message)
                                        "\ncause: " (-> d-e :data :body)
                                        "\ntrace: " (with-out-str (pprint (:trace d-e))))))))]
      (s/assert ::text-to-var res)
      (:CORRESPONDING-VAR res))))

(def general-notes-on-EADS
  (str "Below is an Example Annotated Data Structure (EADS) that defines the structure and intent of what we'd like you to produce.\n"
       "By 'annotated' we mean that in some places, rather than just providing the kind of thing you should produce, we provide that kind of thing AND an associated comment.\n"
       "For example, instead of providing the 'process-id' of some process as a string, we present it as\n"
       "{\"val\" : \"pencil-manufacturing\", \"comment\" : \"This is the top-level process. You can name it as you see fit; don't ask the interviewees.\"}.\n"
       "Such comments are there to help you understand the intent of the example. You do not have to put comments in what you produce, but you could if you'd like.\n"
       "You can use annotations on any property. Where you choose to use them should be entirely independent of where we used them.\n"
       "Where you do use annotations, the 'comment' text should flag something about how you arrived at the 'val', for example, how it is unclear from the interviewees' answer what belongs in the value.\n"
       "For example, if you were asking the interviewees how long shipping takes, they might answer 'it depends'.\n"
       "You could flag this difficulty with an annotation: {\"val\" : \"it depends\", \"comment\" : \"The interviewees did not elaborate.\"}.\n"
       "By using an annotation here, you've flagged something that we can pursue with another agent and the interviewees."))



(defn get-EADS [&_])

(defn respond-with-EADS [&_])
