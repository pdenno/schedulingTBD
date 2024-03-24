(ns scheduling-tbd.surrogate
  "Functions and operators implementing surroage users"
  (:require
   [clojure.edn          :as edn]
   [clojure.string       :as str]
   [scheduling-tbd.db    :as db]
   [scheduling-tbd.llm   :as llm]
   [scheduling-tbd.sutil :as sutil :refer [connect-atm datahike-schema not-nothing register-db db-cfg-map]]
   [taoensso.timbre      :as log]))

(def system-instruction
  "This is the instruction that configures the role of the OpenAI assistant."

  "You manage a company that makes %s.
   You are an expert in production of the company's products and management of its supply chains.
   You help me by answering questions that will allow us to collaborate in building a scheduling systems for your company.
   Your answers typically are short, just a few sentences each.")

(defn name-surrogate
  "Return a keyword in the form of a surrogate :project/id based on the string provided.
   For example, 'Craft Beer' --> :craft-beer-surrogate.
   There is no checking here whether this keyword is unique among :project/id."
  [expertise]
  (-> expertise
      str/lower-case
      (str/replace #"\s+" "-")
      (str "-surrogate")
      keyword))

(defn similar-surrogate?
  "Return a :project/id of a project named similar to the argument if one exists.
   For example :craft-beer-surrogate --> :craft-beer-surrogate-42."
  [pid]
  (let [pid-str (name pid)
        projects (->> (db/list-projects) (mapv name))
        pattern (re-pattern (format "%s(-\\d+)?" pid-str))]
    (some #(when (re-matches pattern %) (keyword %)) projects)))

(defn create-project-db-for-surrogate
  "Create a project DB for the surrogate:
    - pid: project id;  can be (name :surrogate/id)
    - assistant:  the object returned from OpenAI for openai/create-assistant.
    - additional:  a map of other information you want to add to the project db.
    Return the project object."
  ([pid expertise] (create-project-db-for-surrogate pid expertise {}))
  ([pid expertise additional]
   (let [assistant "stubbed!" #_(llm/create-assistant {:name (name pid) :instructions (format system-instruction expertise)})]
     (log/info "Creating new project for" pid)
     (db/create-proj-db!
      {:project/id pid
       :project/name (str expertise " surrogate")}
      [(merge additional
              {:surrogate/id pid
               :surrogate/subject-of-expertise expertise
               :surrogate/system-instruction (format system-instruction expertise)
               :surrogate/assistant-obj-str (str assistant)})]
      {:force? true})
     (db/get-project pid))))

(defn ensure-surrogate
  "If a surrogate with given expertise exists (if its project exists), return it (the project object resolved from the DB).
   Otherwise create and store a project with the given expertise and return it.
   Arguments:
     expertise - a string describing what goes into the surrogate's system instruction;
                 it ends the sentence 'You manage a company that makes %s.
                 For example, any of the :segment/name from the 'How it's Made' DB would work here.
     opts -a map with keys:
            :force? - If you provide, for example, 'craft beer' for the expertise argument and
                      there is a project with id, for example :craft-beer-42, you get that one
                      unless :force? is true, in which case it makes a new one.
                      If you want instead :craft-beer-42, you can provide expertise = 'craft beer 42'."
  [expertise & {:keys [force?]}]
  (let [pid (name-surrogate expertise)]
    (cond force?                     (create-project-db-for-surrogate pid expertise)
          (db/project-exists? pid)   (db/get-project pid)
          (similar-surrogate? pid)   (-> pid similar-surrogate? db/get-project)
          :else                      (create-project-db-for-surrogate pid expertise))))

;;; ToDo: As it says below "A project isn't a map but a vector of things :message/content etc."
;;; Is that okay or clean it up? I think clean it up, but maybe wait to see how complex things get.
;;; There could be LOTS of messages; maybe we don't want those in the project object.
;;; OTOH, I think resolve-db-id can resolve size problems through its filter set.
(defn get-assistant
  "A project isn't a map but a vector of things :message/content etc. Among these is :surrogate/assistant-obj-str.
   This returns the object corresponding to the :surrogate/assistant-obj-str for the given project (a :project/id)."
  [pid]
  (when-let [proj (db/get-project pid)]
    (->> proj
         (some #(when (contains? % :surrogate/id) %))
         :surrogate/assistant-obj-str
         edn/read-string)))

; <================================ Next: Start an Assistant Thread  with "tell me your greatest challenge" message.
(defn start-surrogate
  "Create or recover a surrogate and update the conversation accordingly."
  [expertise]
  (let [proj_(ensure-surrogate expertise)]
    (log/warn "Couldn't get OpenAI API key.")))
