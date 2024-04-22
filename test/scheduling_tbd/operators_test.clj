(ns scheduling-tbd.operators-test
  "Testing of human expert surrogatehis may grow to serve as a surrogate human expert for testing SchedulingTBD's interview process.
   Currently this just tests Wkok's API for OpenAI assistants with the Craft Beer Conversation, data/interviews/2024-01-05-craft-brewing.org."
  (:require
   [clojure.test             :refer [deftest is testing]]
   [promesa.core             :as p]
   [promesa.exec             :as px]
   [scheduling-tbd.db        :as db]
   [scheduling-tbd.llm       :as llm]
   [scheduling-tbd.surrogate :as sur]
   [scheduling-tbd.operators :as op]
   [taoensso.timbre :as log]))

;;; (opt/surrogate-initial-question)
(deftest surrogate-initial-question
  (testing "Some of the activity of making a surrogate and asking the first question."
    (let [pid :sur-plate-glass
          expertise "plate glass"
          itext (format sur/system-instruction expertise) ; Not used here, I just want to print it.
          aid (sur/ensure-surrogate pid (str "SUR " expertise))
          tid (db/get-thread-id pid nil)
          qtext (op/reword-for-agent op/intro-prompt :surrogate)
          prom (px/submit! (fn [] (llm/query-on-thread :tid tid :aid aid :query-text (op/msg-vec2text qtext))))
          res (p/await prom)]
    (log/info "itext =" itext)
    (log/info "qtext =" qtext)
    (log/info "prom =" prom)
    (is (and (string? res)
             (> (count res) 100))))))
