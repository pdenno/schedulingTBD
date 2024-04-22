(ns scheduling-tbd.surrogate
  "Functions and operators implementing surrogate users"
  (:require
   [clojure.string           :as str]
   [datahike.api             :as d]
   [mount.core               :as mount :refer [defstate]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.planner   :as plan]
   [scheduling-tbd.sutil     :as sutil :refer [connect-atm resolve-db-id get-domain]]
   [scheduling-tbd.web.websockets :as ws]
   [taoensso.timbre          :as log]))

(def system-instruction
  "This is the instruction that configures the role of the OpenAI assistant."

  "You manage a company that makes %s.
   You are an expert in production of the company's products and management of its supply chains.
   You help me by answering questions that will allow us to collaborate in building a scheduling systems for your company.
   Your answers typically are short, just a few sentences each.")

#_(defn similar-surrogate?
  "Return a :project/id of a project named similar to the argument if one exists.
   For example :craft-beer-surrogate --> :craft-beer-surrogate-42."
  [pid]
  (let [pid-str (name pid)
        projects (->> (db/list-projects) (mapv name))
        pattern (re-pattern (format "%s(-\\d+)?" pid-str))]
    (some #(when (re-matches pattern %) (keyword %)) projects)))

(defn surrogate-init-problem
  "Create the initial :project/planning-problem for a surrogate."
  [pid pname]
  (let [pid-sym (-> pid name symbol)]
    `{:problem/domain :process-interview
      :problem/goal-string ~(format "(characterize-process %s)" (name pid))
      :problem/state-string ~(format "#{(proj-id %s) (surrogate %s) (proj-name \"%s\")}" pid-sym pid-sym pname)}))

(defn ensure-surrogate
  "If a surrogate with given expertise exists (if its project exists), return it (the project object resolved from the DB).
   Otherwise create and store a project with the given expertise and the OpenAI assistant object.
   In either case, it returns the Openai assistant object ID associated with the pid.
     pid - the project ID (keyword) of a project with an established DB."
  [pid pname]
  (or (db/get-assistant-id pid nil)
      (let [conn (connect-atm pid)
            eid (db/project-exists? pid)
            proj-info (resolve-db-id {:db/id eid} conn :keep-set #{:project/name})
            [_ _ expertise] (re-matches #"(SUR )?(.*)" (:project/name proj-info)) ; ToDo: Ugh!
            expertise (str/lower-case expertise)
            instructions (format system-instruction expertise)
            assist (llm/make-assistant :name (str expertise " surrogate") :instructions instructions :metadata {:usage :surrogate})
            aid    (:id assist)
            thread (llm/make-thread {:assistant-id aid :metadata {:usage :surrogate}})
            prob (surrogate-init-problem pid pname)] ; Surrogates have just one thread.
        (log/info "Made assistant" aid "for instructions" instructions)
        (d/transact conn {:tx-data [{:db/id (db/project-exists? pid)
                                     :project/planning-problem prob
                                     :project/surrogate {:surrogate/id pid
                                                         :surrogate/subject-of-expertise expertise
                                                         :surrogate/system-instruction instructions
                                                         :surrogate/assistant-id aid
                                                         :surrogate/thread-id (:id thread)}}]})
        (db/get-assistant-id pid))))

;;; (sur/start-surrogate {:product "plate glass" :client-id (ws/recent-client!)})
(defn start-surrogate
  "Create or recover a surrogate and ask client to :reload-proj. :reload-proj will start the planner; not done here directly.
     product - a string describing what product type the surrogate is going to talk about (e.g. 'plate glass').
               Any of the :segment/name from the 'How it's Made' DB would work here.
  "
  [{:keys [product client-id]} & {:keys [force?] :or {force? true}}] ; ToDo: handle force?=false, See similar-surrogate?
  (log/info "Start a surrogate: product =" product)
  (let [pid (as-> product ?s (str/trim ?s) (str/lower-case ?s) (str/replace ?s #"\s+" "-") (str "sur-" ?s) (keyword ?s))
        pname (as->  product ?s (str/trim ?s) (str/split ?s #"\s+") (map str/capitalize ?s) (interpose " " ?s) (conj ?s "SUR ") (apply str ?s))
        pid (db/create-proj-db! {:project/id pid :project/name pname} {} {:force? force?})]
    (try
      (ensure-surrogate pid pname)
      (ws/send-to-chat {:dispatch-key :reload-proj :client-id client-id  :promise? nil
                        :new-proj-map {:project/name pname :project/id pid}})
      (catch Exception e
        (log/warn "Failed to start surrogate.")
        (log/error "Error starting surrogate:" e)))))

;;; ----------------------- Starting and stopping -----------------------------------------
(defn init-surrogates! []
  (ws/register-ws-dispatch :start-surrogate start-surrogate)
  :surrogates-started)

(defn stop-surrogates! []
  :done)

(defstate surrogates
  :start (init-surrogates!)
  :stop (stop-surrogates!))
