(ns scheduling-tbd.surrogate-test
  "Testing of human expert surrogatehis may grow to serve as a surrogate human expert for testing SchedulingTBD's interview process.
   Currently this just tests Wkok's API for OpenAI assistants with the Craft Beer Conversation, data/interviews/2024-01-05-craft-brewing.org."

  (:require
   [clojure.edn             :as edn]
   [clojure.spec.alpha      :as s]
   [clojure.string          :as str]
   [clojure.test            :refer [deftest is testing]]
   [datahike.api            :as d]       ; Store thread.
   [scheduling-tbd.sutil    :refer [get-api-key]]
   [wkok.openai-clojure.api :as openai]))


;;; A challenge for this is that the surrogate is free to whatever it likes but we don't yet have
;;; all the tools needed to make sense of what it is saying (the meta-theoretical tools, etc.).

(def system-instruction
  "This is the instruction that configures the role of the OpenAI assistant."

  "You manage a company that makes %s.
   You are an expert in production of the company's products and management of its supply chains.
   You help me by answering questions that will allow us together to build a scheduling systems for your company.
   Your answers typically are short, just a few sentences each.")

(def beer-example
  "Example data for running an interview (sans planner computing the questions)."
  {:surrogate/id "craft-beer-1"
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
