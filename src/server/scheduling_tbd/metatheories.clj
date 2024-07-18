(ns scheduling-tbd.metatheories
  "Prompts for non-domain characterization of user text."
  (:require
   [scheduling-tbd.llm           :as llm]
   [scheduling-tbd.sutil         :refer [connect-atm api-credentials default-llm-provider]]
   [clojure.pprint               :refer [cl-format pprint]]
   [clojure.string               :as str]
   [taoensso.timbre              :as log]))
