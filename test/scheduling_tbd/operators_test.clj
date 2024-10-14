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
    (sur/ensure-surrogate :sur-plate-glass "SUR plate glass")
    (let [pid :sur-plate-glass
          itext (format sur/system-instruction "plate glass") ; Not used here, I just want to print it.
          {:surrogate/keys [assistant-id thread-id]} (db/surrogate-info pid)
          qtext "Describe your most significant scheduling problem in a few sentences."
          prom (px/submit! (fn [] (llm/query-on-thread :aid assistant-id :tid thread-id :query-text qtext)))
          res (p/await prom)]
    (log/info "itext =" itext)
    (log/info "qtext =" qtext)
    (log/info "prom =" prom)
    (is (and (string? res)
             (> (count res) 100))))))
