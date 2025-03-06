(ns scheduling-tbd.interviewing.response-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.datafy                :refer [datafy]]
   [clojure.core.unify            :as uni]
   [clojure.pprint                :refer [cl-format pprint]]
   [clojure.spec.alpha            :as s]
   [clojure.string                :as str]
   [jsonista.core                 :as json]
   [scheduling-tbd.agent-db       :as adb]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.telemere             :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

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
  ([text] (text-to-var text {}))
  ([text {:keys [asking-role]}]
   (if (good-for-var? text)
     (make-var-from-string text)
     (let [res (try (-> (adb/query-agent :text-to-var
                                         (format "{\"INPUT-TEXT\" : \"%s\"}" text)
                                         (when asking-role {:asking-role asking-role}))
                        json/read-value
                        (update-keys keyword))
                    (catch Exception e
                      (let [d-e (datafy e)]
                        (log! :warn (str "text-to-var failed\n "
                                         "\nmessage: " (-> d-e :via first :message)
                                         "\ncause: " (-> d-e :data :body)
                                         "\ntrace: " (with-out-str (pprint (:trace d-e))))))))]
       (s/assert ::text-to-var res)
       (:CORRESPONDING-VAR res)))))

(defn deep-keys [obj]
  (cond (map? obj)      (reduce-kv (fn [m k v] (assoc m (keyword k) (deep-keys v))) {} obj)
        (vector? obj)   (mapv deep-keys obj)
        :else           obj))

;;; ====================== Shared by use of tags :process :data :resources :optimality ====================
(defn analyze-warm-up--dispatch [tag & _] tag)

;;; The methods for this are in the interviewing/domain directory.
(defmulti analyze-warm-up #'analyze-warm-up--dispatch)

(defn eads-response--dispatch [tag & _] tag)

;;; The methods for this are in the interviewing/domain directories.
;;; Returns a PHASE-2-EADS message. (See for example the one for :process in process_analysis.clj.
(defmulti eads-response! #'eads-response--dispatch)
