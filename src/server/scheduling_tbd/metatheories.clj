(ns scheduling-tbd.metatheories
  "Prompts for non-domain characterization of user text."
  (:require
   [scheduling-tbd.llm           :as llm :refer [chat2clj]]
   [scheduling-tbd.sutil         :refer [connect-atm get-api-key]]
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]))
