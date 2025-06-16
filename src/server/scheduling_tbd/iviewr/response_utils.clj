(ns scheduling-tbd.iviewr.response-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [cheshire.core                 :as ches]
   [clojure.datafy                :refer [datafy]]
   [clojure.core.unify            :as uni]
   [clojure.pprint                :refer [pprint]]
   [clojure.spec.alpha            :as s]
   [clojure.string                :as str]
   [scheduling-tbd.agent-db       :as adb]
   [scheduling-tbd.db             :as db]
   [scheduling-tbd.specs          :as specs]
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
  (s/assert ::specs/client-id client-id)
  (ws/send-to-client {:promise? false
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
                        ches/parse-string
                        (update-keys keyword))
                    (catch Exception e
                      (let [d-e (datafy e)]
                        (log! :warn (str "text-to-var failed\n "
                                         "\nmessage: " (-> d-e :via first :message)
                                         "\ncause: " (-> d-e :data :body)
                                         "\ntrace:\n" (with-out-str (pprint (:trace d-e))))))))]
       (s/assert ::text-to-var res)
       (:CORRESPONDING-VAR res)))))
