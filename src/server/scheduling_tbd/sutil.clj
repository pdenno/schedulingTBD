(ns scheduling-tbd.sutil
  "Server utilities."
  (:require
   [cheshire.core            :as ches]
   [clojure.java.io          :as io]
   [clojure.string           :as str]
   [datahike.api             :as d]
   [datahike.pull-api        :as dp]
   [taoensso.telemere        :refer [log!]])
  (:import java.time.Instant))

(def ^:diag diag (atom nil))

;(def llm-provider "Default provider to use. Choices are #{:openai :azure}." :openai) ; Values are azure and :openai
(def default-llm-provider "Default provider to use. Choices are #{:openai :azure}." (atom :openai)) ; Values are azure and :openai

(defn api-credentials [provider]
  (let [res (case provider
                :openai {:api-key (System/getenv "OPENAI_API_KEY")}
                :azure  {:api-key (System/getenv "AZURE_OPENAI_API_KEY")
                         :api-endpoint "https://myopenairesourcepod.openai.azure.com"
                         :impl :azure})]
    (when-not (:api-key res)
      (if (= provider :openai)
        (log! :error "Specify an API key in the environment variable OPENAI_API_KEY")
        (log! :error "Specify an API key in the environment variable AZURE_OPENAI_API_KEY")))
    res))

(defonce databases-atm (atom {}))

(defn db-ids [] (-> @databases-atm keys sort vec))

(defn register-db
  "Add a DB configuration."
  [k config]
  (log! :debug (str "Registering DB " k "config = " config))
  (swap! databases-atm #(assoc % k config)))

(defn deregister-db
  "Add a DB configuration."
  [k]
  (log! :info (str "Deregistering DB " k))
  (swap! databases-atm #(dissoc % k)))

(def db-template
  "Hitchhiker file-based DBs follow this form."
  {:store {:backend :file :path "Provide a value!"} ; This is path to the database's root directory
   :keep-history? false
   :base-dir "Provide a value!"                     ; For convenience, this is just above the database's root directory.
   :recreate-dbs? false                             ; If true, it will recreate the system DB and project directories too.
   :schema-flexibility :write})

;;; https://cljdoc.org/d/io.replikativ/datahike/0.6.1545/doc/datahike-database-configuration
(defn db-cfg-map
  "Return a datahike configuration map for argument database (or its base).
     id   - a keyword uniquely identifying the DB in the scope of DBs.
     type - the type of DB configuration being make: (:project, :system, or :him, so far)"
  [{:keys [type id in-mem?]}]
  (when (and (= :project type) (not id)) (throw (ex-info "projects need an ID." {})))
  (let [base-dir (or (-> (System/getenv) (get "SCHEDULING_TBD_DB")) ; "/opt/scheduling" typically.
                     (throw (ex-info (str "Set the environment variable SCHEDULING_TBD_DB to the directory containing SchedulingTBD databases."
                                          "\nCreate directories 'projects' and 'system' under it.") {})))
        db-dir (->> (case type
                      :system           "/system"
                      :project          (str "/projects/" (name id) "/db/")
                      :planning-domains "/planning-domains"
                      :him              "/etc/other-dbs/him")
                    (str base-dir))]
    (cond-> db-template
      true            (assoc :base-dir base-dir)     ; This is not a datahike thing.
      (not in-mem?)   (assoc :store {:backend :file :path db-dir})
      in-mem?         (assoc :store {:backend :mem :id (name id)}))))

(defn connect-atm
  "Return a connection atom for the DB.
   Throw an error if the DB does not exist and :error? is true (default)."
  [k & {:keys [error?] :or {error? true}}]
  (if-let [db-cfg (get @databases-atm k)]
    (if (d/database-exists? db-cfg)
      (d/connect db-cfg)
      (when error?
        (throw (ex-info (str "Could not connect to DB: " k) {:key k}))))
    (when error?
      (throw (ex-info (str "No such DB: " k) {:key k})))))

(defn datahike-schema
  "Create a Datahike-compatible schema from map-type schema with notes such as :mm/info."
  [schema]
  (reduce-kv (fn [r k v]
               (conj r (-> v
                           (dissoc :mm/info)
                           (assoc :db/ident k))))
             []
             schema))
;;; ToDo:
;;;  - cljs complains about not finding x/element-nss, which I don't see in the  0.2.0-alpha8 source at all.
;;;    (Yet it does work in clj!) I suppose reading xml isn't something I need in cljs, but it would be
;;;    nice to know what is going on here.
;;; ToDo: Get some more types in here, and in implementation generally.
(defn db-type-of
  "Return a Datahike schema :db/valueType object for the argument"
  [obj]
  (cond (string? obj)  :db.type/string
        (number? obj)  :db.type/number
        (keyword? obj) :db.type/keyword
        (map? obj)     :db.type/ref
        (boolean? obj) :db.type/boolean))

;;; This seems to cause problems in recursive resolution. (See resolve-db-id)"
(defn db-ref?
  "It looks to me that a datahike ref is a map with exactly one key: :db/id."
  [obj]
  (and (map? obj) (= [:db/id] (keys obj))))

;;; {:db/id 3779}
(defn resolve-db-id
  "Return the form resolved, removing properties in filter-set,
   a set of db attribute keys, for example, #{:db/id}."
  [form conn-atm & {:keys [keep-set drop-set]
                    :or {drop-set #{:db/id}
                         keep-set #{}}}]
  (letfn [(resolve-aux [obj]
            (cond
              (db-ref? obj) (let [res (dp/pull @conn-atm '[*] (:db/id obj))]
                              (if (= res obj) nil (resolve-aux res)))
              (map? obj) (reduce-kv (fn [m k v]
                                      (cond (drop-set k)                                    m
                                            (and (not-empty keep-set) (not (keep-set k)))   m
                                            :else                                           (assoc m k (resolve-aux v))))
                                    {}
                                    obj)
              (vector? obj)      (mapv resolve-aux obj)
              (set? obj)    (set (mapv resolve-aux obj))
              (coll? obj)        (map  resolve-aux obj)
              :else  obj))]
    (resolve-aux form)))

(defn root-entities
  "Return a sorted vector of root entities (natural numbers) for all root entities of the DB."
  [conn-atm]
  (-> (d/q '[:find [?e ...] :where
             [?e]
             (not [_ _ ?e])]
           @conn-atm)
      sort
      vec))

(defn nspaces
  "Return a string of n spaces."
  [n]
  (reduce (fn [s _] (str s " ")) "" (range n)))

(defn elide
  "Return a string no longer than n where the last 3 is ellipsis '...' if the string is > n long."
  [s n]
  (let [cnt (count s)]
    (cond (> n cnt)   s
          (< n 3)     ""
          :else (str (subs s 0 (- n 3)) "..."))))

(defn not-nothing
  "Returns true if it is a collection and not empty or something else non-nil"
  [x]
  (if (seq? x)
    (not-empty x)
    x))

;;; https://gist.github.com/lnostdal/cc956e2a80dc49d8097b7c950f7213bd
(defn move-file
  "Move file/directory from source to target. Both are full pathnames.
   This will also replace the target file if it exists since REPLACE_EXISTING is included in the options at the end."
  [source target]
  (let [source-file (java.nio.file.Paths/get (java.net.URI/create (str "file://" source)))
        target-file (java.nio.file.Paths/get (java.net.URI/create (str "file://"  target)))]
    (java.nio.file.Files/move source-file target-file
                              (into-array java.nio.file.CopyOption
                                          [(java.nio.file.StandardCopyOption/ATOMIC_MOVE)
                                           (java.nio.file.StandardCopyOption/REPLACE_EXISTING)]))))

;;; Keep this around for a while; it might get used eventually!
#_(defmacro report-long-running
  "Return the string from writing to *out* after this runs in a future."
  [[timeout] & body]
  `(-> (p/future (with-out-str ~@body))
       (p/await ~timeout)
       (p/then #(log! :info (str "Long-running: " %)))
       (p/catch #(log! :warn (str "Long-running (exception): " %)))))

(defn chat-status
  "Create a string to explain in the chat the error we experienced."
  ([s] (chat-status s nil))
  ([s err]
   (if (instance? Throwable err)
     (let [m (Throwable->map err)]
       (if-let [msg (-> m :via first :message)]
         (if-let [data (:data m)]
           (str ": " msg  "\ndata:" data)
           (str s ": " msg))
         s))
     s)))

(defn yes-no-unknown
  "Return :yes :no or :unknown based on lexical analysis of the argument answer text."
  [s]
  (cond (or (= s "yes") (= s "Yes") (= s "Yes.") (re-matches #"(?i)\s*yes\s*" s)) :yes
        (or (= s "no")  (= s "No")  (= s "No.")  (re-matches #"(?i)\s*no\s*"  s)) :no
        :else :unknown))

(defn markdown2html
  "Do heuristic light modification to the argument text to make it more like HTML.
   Specifically:
     - Change: **bold** to <b>bold</b>.
   This is mostly for use with chatbots that return markup."
  [s]
  (let [lines (for [line (str/split-lines s)]
                (let [[success pre bold post] (re-matches #"(.*)\*\*(.+)\*\*(.*)" line)] ; ToDo: I can't put \- in the bold stuff.
                  (if success
                    (str pre "<b>" bold "</b>" post "\n")
                    (str line "\n")))) ; ToDo: interpose!
        last (last lines)
        others (butlast lines)]
    (str (apply str others)
         (subs last 0 (dec (count last))))))

(defn remove-preamble
  "The LLM might put text and markup around the answer, return the answer without this crap."
  [response]
  (let [response (str/replace response #"\s" " ")]
    (cond (re-matches #".*```clojure.*" response)
          (let [pos (str/index-of response "```clojure")
                response (subs response (+ pos 10))
                pos (str/index-of response "```")]
            (subs response 0 pos))

          (re-matches #".*```json.*" response)
          (let [pos (str/index-of response "```json")
                response (subs response (+ pos 7))
                pos (str/index-of response "```")]
            (subs response 0 pos))
          :else response)))

;;; This became complicated once I couldn't use strict schema results.
(defn output-struct2clj
  "Translate the OpenAI API output structure (a string) to a map with keyword keys."
  [s-in]
  (try
    (let [s (remove-preamble s-in)]
      (update-keys (ches/parse-string s) keyword))
    (catch Exception _e
      (throw (ex-info  "Could not read object returned from OpenAI (should be a string):" {:s-in s-in })))))

(defn clj2json-pretty
  "Return a pprinted string for given clojure object."
  [obj]
  (ches/generate-string obj {:pretty true}))

#_(defn same-EADS-instructions?
  "Return true if the argument eads-instructions (an EDN object) is exactly what the system already maintains."
  [eads-instructions]
  (let [id (-> eads-instructions :EADS :EADS-id)
        [ns nam] ((juxt namespace name) id)]
    (assert (and ns nam))
    (let [eads-json-fname (str "resources/agents/iviewrs/EADS/" ns "/" nam ".edn")
          old-text (if (.exists (io/file eads-json-fname)) (slurp eads-json-fname) "")
          new-text (clj2json-pretty eads-instructions)]
      (= old-text new-text))))

(defn update-resources-EADS-json!
  "Update the resources/agents/iviewrs/EADS directory with a (presumably) new JSON pprint of the argument EADS instructions.
   These are needed by the orchestrator; they are put in its vector store."
  [eads-instructions]
  (let [id (-> eads-instructions :EADS :EADS-id)
        eads-json-fname (str "resources/agents/iviewrs/EADS/" (name id) ".json")]
    (spit eads-json-fname (clj2json-pretty eads-instructions))))
