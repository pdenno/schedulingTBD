(ns scheduling-tbd.surrogate-test
  "Testing of human expert surrogate. This may grow to serve as a surrogate human expert for testing SchedulingTBD's interview process.
   Currently this just tests Wkok's API for OpenAI assistants with the Craft Beer Conversation, data/interviews/2024-01-05-craft-brewing.org."
  (:require
   [clojure.test             :refer [deftest is testing]]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.surrogate :as sur]
   [taoensso.timbre :as log]))

(def ^:diag diag (atom nil))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'pinv   'scheduling-tbd.domain.process.p-interview)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  (safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'plan   'scheduling-tbd.planner)
  (safe-alias 'plant  'scheduling-tbd.planner-test)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'openai 'wkok.openai-clojure.api))

;;; A challenge for this is that the surrogate is free to whatever it likes but we don't yet have
;;; all the tools needed to make sense of what it is saying (the meta-theoretical tools, etc.).
(def beer-example
  "Example data for running an interview (sans planner computing the questions)."
  {:project/name "Craft beer surrogate"
   :surrogate/id :craft-beer-surrogate-1
   :surrogate/subject-of-expertise "craft beer"

   ;; This is data/interviews/2024-01-05-craft-brewing.org with some improvements. There is more in that interview, but
   ;; without some analysis of what the surrogate is saying, I am reluctant to push this further that what is shown here.
   :tbd/prompts
   ["Please describe the greatest scheduling challenge you face."

    ;; Background info from a "well-known process" investigation allows this:
    "Is it fair to say that the production steps involve malting, milling, mashing, lautering, boiling, fermenting, brighting, and bottling?"

    "What is the principal resource used in each of these steps?"

    ;; Use of 'one product type' might make this confusing. This question can be adapted using background info.
    "For each of these resources, just one product type can use it at a time, right?"

    "How long do each of these steps take?"

    "How many product types do you run simultaneously?"

    ;; ToDo: This will need to be adapted to the actual steps that come back. For example the term "conditioning" might not appear.
    ;; These should have been put in the project DB.
    "By a wide margin, the lengthy steps are fermenting and conditioning.
     As a first attempt at specifying a scheduling system for your operations, would it be okay if
     we consider the first few steps (milling, mashing, lautering, boiling, and cooling) as one big step called 'preparation'?
     (Just a Y/N answer would be fine.)"

    ;; ToDo: Will it say 'no' here? Risk of TBD looking desultory here. For the time being, I won't go further.
    "Your processes do not have a place to store work-in-process product?
     The product must go from one step to the next? Is that correct? (Just a Y/N answer would be fine.)"]})


;;; (openai/list-assistants {:limit 3})
;;; ToDo: First check that it doesn't exist.

(defn create-project-db-for-surrogate
  "Create a project DB for the surrogate:
    - pid: project id;  can be (name :surrogate/id)
    - assistant:  the object returned from OpenAI for openai/create-assistant.
    - additional:  a map of other information you want to add to the project db.
    Return its root entity id (an integer)."
  [pid pname assistant additional]
  (db/create-proj-db!
   {:project/id pid
    :project/name pname}
   [(merge additional
          {:surrogate/id pid
           :surrogate/openai-obj-str (str assistant)})]
   {:force? true}))

(def system-instruction
  "This is the instruction that configures the role of the OpenAI assistant."

  "You manage a company that makes %s.
   You are an expert in production of the company's products and management of its supply chains.
   You help me by answering questions that will allow us together to build a scheduling systems for your company.
   Your answers typically are short, just a few sentences each.")

;;; Next two need investigation. No more delete/assistant-openai (maybe add something to adb?)
#_(deftest assistant-tests
  (testing "Testing code to make an assistant."
    (let [aid-atm (atom nil)]
      (try (let [expertise "plate glass"
                 instructions (format system-instruction expertise)
                 asst (llm/make-assistant :name (str expertise " surrogate") :instructions instructions :metadata {:usage :surrogate})
                 aid  (:id asst)
                 tid (-> (llm/make-thread {:assistant-id aid :metadata {:usage :surrogate}}) :id)
                 answer (adb/query-on-thread :aid aid :tid tid :query-text "Using only two words, name what your company makes.")]
             (reset! aid-atm aid)
             (is (= answer "Plate glass.")))
           (finally (llm/delete-assistant-openai @aid-atm))))))

#_(deftest query-on-thread-tests
  (testing "Testing code to make an assistant."
    (let [aid-atm (atom nil)
          instructions "No matter what you are asked, you respond with a sentence of just one word: \"Great!\""]
      (try (let [expertise "plate glass"
                 asst (llm/make-assistant :name (str expertise " surrogate") :instructions instructions :metadata {:usage :surrogate})
                 aid  (:id asst)
                 tid (-> (llm/make-thread {:assistant-id aid :metadata {:usage :surrogate}}) :id)
                 answer (adb/query-on-thread :aid aid :tid tid :query-text "How is it going?"
                                             :tries 2
                                             :test-fn (fn [x] (log/info "You said:" x) (= x "Great!")))]
             (reset! aid-atm aid)
             (is (nil?  answer)))
      (finally (llm/delete-assistant-openai @aid-atm))))))
