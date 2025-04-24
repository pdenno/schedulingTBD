(ns scheduling-tbd.interviewing.response-utils
  "Implementation of the action of plans. These call the LLM, query the user, etc."
  (:refer-clojure :exclude [send])
  (:require
   [clojure.datafy                :refer [datafy]]
   [clojure.core.unify            :as uni]
   [clojure.edn                   :as edn]
   [clojure.java.io               :as io]
   [clojure.pprint                :refer [pprint]]
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

(defn get-iviewr-info [cid]
  (-> "agents/iviewrs/iviewr-infos.edn"
      io/resource
      slurp
      edn/read-string
      (get cid)))

(defn get-warm-up-q [cid]
  (-> cid get-iviewr-info :warm-up-question))

(defn strip-annotations
  "Remove the annotations from the EADS."
  [obj]
  (cond (and (map? obj) (contains? obj :val) (contains? obj :comment))  (:val obj)
        (map? obj)                                                      (reduce-kv (fn [m k v] (assoc m k (strip-annotations v))) {} obj)
        (vector? obj)                                                   (mapv strip-annotations obj)
        :else                                                           obj))

(defn key-vals
  "Return a collection of keys for which the value is a key."
  [m]
  (let [res (atom #{})]
    (letfn [(kv [obj]
              (cond  (map? obj)     (doseq [[k v] obj] (if (keyword? v) (swap! res conj k) (kv v)))
                     (vector? obj)  (doseq [v obj] (kv v))))]
      (kv m)
      @res)))

;;; ToDo: Use of key-vals is questionable.
(defn ds2clj
  "Walk through the data structure, comparing it to the EADS and update map value strings to keywords where appropriate.
   The term 'data structure' refers to the map structure interviewers create from an EADS."
  [ds]
  (let [msg (or (-> ds :EADS-used keyword db/get-eads not-empty)
                (throw (ex-info "No such EADS:" {:name (:EADS-used ds)})))
        key-val? (-> msg :EADS strip-annotations key-vals)]
    (letfn [(ds2 [obj]
              (cond (map? obj)      (reduce-kv (fn [m k v] (assoc m k (if (key-val? k) (keyword v) (ds2 v)))) {} obj)
                    (vector? obj)   (mapv ds2 obj)
                    :else           obj))]
      (ds2 ds))))

;;; ====================== Shared by use of tags :process :data :resources :optimality ====================
(defn dispatch-by-cid [tag & _]
  (if (#{:process :data :resources :optimality} tag)
    tag
    (throw (ex-info "Bad tag" {:tag tag}))))

;;; The methods for this are in the interviewing/domain directory.
(defmulti analyze-warm-up        #'dispatch-by-cid)
(defmulti conversation-complete? #'dispatch-by-cid)
