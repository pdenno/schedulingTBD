(ns scheduling-tbd.interviewing.domain.process.job-shop-c
  "(1) Define the an example annotated data structure (EADS) to provide to the interviewer for a job-shop scheduling problem.
       As the case is with job-shop problems, this structure defines work to be performed in typical job.
   (2) Define well-formedness constraints for this structure. These can also be used to check the structures produced by the interviewer.

   Note: We don't load this code at system startup. When you compile it, it writes the EADS to resources/EADS/job-shop-c.txt"
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))

(s/def :job-shop-c/EADS-message (s/keys :req-un [::message-type ::interview-objective ::interviewer-agent ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent #(= % :process))

(s/def ::comment string?) ; About annotations

(s/def ::EADS (s/keys :req-un [::EADS-id ::multiple-production-lines? ::job-level-processes]))
(s/def ::EADS-id #(= % :process/job-shop--classifiable))
(s/def ::multiple-production-lines? (s/or :normal :multiple-production-lines?/val :annotated ::annotated-multiple-production-lines?))
(s/def :multiple-production-lines?/val boolean?)
(s/def ::annotated-multiple-production-lines? (s/keys :req-un [:multiple-production-lines?/val ::comment]))
(s/def ::job-level-processes (s/or :normal :job-level-processes/val :annotated ::annotated-job-level-processes))
(s/def :job-level-processes/val  (s/coll-of ::process :kind vector?))
(s/def ::annotated-job-level-processes (s/keys :req-un [:job-level-processes/val ::comment]))

;;; --------------------------------- This is all borrowed from flow_shop.clj
(s/def ::process (s/keys :req-un [::process-id ::subprocesses] :opt-un [::duration ::inputs ::outputs ::resources]))
(s/def ::process-id (s/or :normal :process-id/val :annotated ::annotated-process-id))
(s/def ::annotated-process-id (s/keys :req-un [:process-id/val ::comment]))
(s/def :process-id/val string?)

(s/def ::thing (s/or :normal :thing/val :quantified ::thing-with-quantity :originated ::thing-with-origin))
(s/def ::thing-with-quantity (s/keys :req-un [::item-id ::quantity]))
(s/def ::thing-with-origin (s/keys :req-un [::item-id ::from]))
(s/def :thing/val string?)

(s/def ::item-id (s/or :normal :item-id/val :annotated ::annotated-item-id))
(s/def ::annotated-item-id (s/keys :req-un [:item-id/val ::comment]))
(s/def :item-id/val string?)

(s/def ::quantity (s/or :normal :quantity/val :annotated ::annotated-quantity))
(s/def :quantity/val (s/or :normal (s/keys :req-un [::units ::value-string]) :annotated ::annotated-quantity))
(s/def ::units (s/or :normal :units/val  :annotated ::annotated-units))
(s/def ::annotated-quantity (s/keys :req-un [:units/val ::comment]))
(s/def :units/val string?)

(s/def ::units (s/or :normal :units/val :annotated ::annotated-units))
(s/def :units/val string?)
(s/def ::annotated-units (s/keys :req-un [:units/val ::comment]))
(s/def ::value-string (s/or :normal :value-string/val :annotated ::annotated-value-string))
(s/def ::annotated-value-string (s/keys :req-un [:value-string/val ::comment]))
(s/def :value-string/val string?)

(s/def ::inputs (s/or :normal :inputs/val :annotated ::annotated-inputs))
(s/def :inputs/val (s/coll-of :input/val :kind vector?))
(s/def :input/val  (s/or :simple ::thing :with-origin ::input-with-origin))
(s/def ::input-with-origin (s/keys :req-un [::item-id ::from]))
(s/def ::annotated-inputs (s/keys :req-un [:inputs/val ::comment]))
(s/def ::from (s/or :normal :from/val :annotated ::annotated-from))
(s/def :from/val string?)
(s/def ::annotated-from (s/keys :req-un [:from/val ::comment]))

(s/def ::outputs (s/or :normal :outputs/val :annotated ::annotated-outputs))
(s/def :outputs/val (s/coll-of :input/val :kind vector?))
(s/def :input/val  ::thing)
(s/def ::annotated-outputs (s/keys :req-un [:outputs/val ::comment]))

(s/def ::resources (s/or :normal :resources/val :annotated ::annotated-resources))
(s/def :resources/val (s/coll-of string? :kind vector?))
(s/def ::annotated-resources (s/keys :req-un [:resources/val ::comment]))

(s/def ::subprocesses (s/or :normal :subprocesses/val :annotated ::annotated-subprocesses))
(s/def :subprocesses/val (s/coll-of ::process :kind vector?))
(s/def ::annotated-subprocesses (s/keys :req-un [:subprocesses/val ::comment]))

(s/def ::item-id string?)
(s/def ::quantity (s/keys :req-un [::units ::value-string]))
(s/def ::units string?)
(s/def ::value-string string?)

;;; (s/explain :job-shop-c/EADS-message jshopc/job-shop-c)
(def job-shop-c
  "A pprinted (JSON?) version of this is what we'll provide to the interviewer at the start of Phase 2 of a job-shop-c problem."
  {:message-type :EADS-INSTRUCTIONS
   :interviewer-agent :process
   :interview-objective (str "These EADS-INSTRUCTIONS assumes that\n"
                             "  (1) the interviewees' production processes are organized as a job shop, and\n"
                             "  (2) each of the jobs they undertake can be classified as following one of a small set of process plans.\n"
                             "The purpose of these EADS-INSTRUCTIONS are to describe these process plans.\n"
                             "There are a few ways to go about this:\n"
                             "  *  One way is to describe a single encompassing flow-shop-like process where some tasks are optional for some jobs.\n"
                             "     In this arrangement, we'd expect information about what processes are optional to accompany each job.\n"
                             "     In this arrangement, it is not your responsibility to relate jobs to processes; just capture the processes.\n"
                             "\n"
                             "  *  A second way is to describe a collection of distinct flow-shop-like process plans, one of which applies to every job they will encounter.\n"
                             "     You would choose this if there are permutations in the ordering of tasks among the jobs.\n"
                             "     (You will see from the EADS data structure below, that it does not provide a way describe these permuatations, thus the proliferation of process plans.)\n"
                             "     (By the way, you are free to mark tasks as optional in this approach, just as you might in the single encompassing flow-shop-like process described in the previous bullet.)\n"
                             "     You would choose this approach if, for example, they make a few very different products (like, for example, t-shirts and running shoes).\n"
                             "     It is possible that if they do make a few very different products, that they actually are talking about scheduling multiple production lines!\n"
                             "     There is a boolean in the EADS, 'multiple-production-lines?' that you can set to true if you learn that they have in mind scheduling multiple flow-shop-like production lines;\n"
                             "     We'll deal with this problem later, in the resources interview."
                             "\n"
                             "Note that the data structures used to define processes in these EADS instructions are like the one in the process/flow-shop EADS instructions.\n"
                             "The principal differences are that (1) this one allows for multiple job-level processes, and (2) this one has the multiple-production-lines? property described above.")
   :EADS
   {:EADS-id :process/job-shop--classifiable
    :multiple-production-lines? {:val false,
                                 :comment (str "This property is true only in the case that you learn that they run multiple flow-shop-like production lines that each support distinct process flows \n"
                                               "appropriate for only some jobs.")}
    :job-level-processes {:comment (str "This is a list of flow-shop-like processes as described in the interview-objectives above.\n"
                                        "The list only has one job-level process in it, but in your interveiw you can define many as described in the interview-objectives.")
                          :val [{:process-id {:val "pencil-manufacturing",
                                              :comment "This is the top-level process. You can name it as you see fit; don't ask the interviewees."}

                                 :inputs {:val ["graphite", "clay", "water", "cedar wood", "metal", "eraser material", "paint"],
                                          :comment "These are all the raw materials used to make the product. You can figure this out by looking at all the raw materials in the leaf processes."}

                                 :outputs {:val [{:item-id "finished pencils",
                                                  :quantity {:units "finished pencils" :value-string "100000"}}]
                                           :comment (str "inputs and outputs can either be simple strings like we used above, 'graphite', clay..., or objects like this, with an 'item-id' and 'quantity'.\n"
                                                         "Use disgression (mindful of the questioning budget) about where you ask for quantities. Start simple and pursue details were the budget allows.")}

                                :resources {:val ["extruder", "kiln", "milling machine", "glue applicator", "shaping machine"],
                                            :comment "Resources, unlike inputs, are durable and reusable. Do not ask about quantities of resources; that's a conversation for another interviewer."},

                                 :duration {:val {:units "hours", :value-string "4"},
                                            :comment "We use a string for 'value-string' in case interviewees answer it something like 'it varies'"}

                                 :subprocesses [{:process-id "graphite-core-production",
                                                 :inputs ["graphite", "clay", "water"],
                                                 :outputs [{:item-id "graphite core"
                                                            :quantity {:units "graphite cores" :value-string "100000"}}],
                                                 :resources ["mixer", "extruder", "kiln"],
                                                 :subprocesses [{:process-id "mix-graphite-and-clay",
                                                                 :inputs ["graphite", "clay", "water"],
                                                                 :outputs [{:item-id "graphite-clay paste",
                                                                            :quantity {:units "liters", :value-string "100"}}],
                                                                 :resources ["mixer"],
                                                                 :duration  {:units "hours", :value-string "1"}
                                                                 :subprocesses {:val []
                                                                                :comment (str "We use empty array val values to signify that we don't think there are any interesting sub-process from the standpoint of scheduling.\n"
                                                                                              "Of course, this could be updated later if subsequent discussion suggests we are wrong.")}}

                                                                {:process-id "extrude-core",
                                                                 :inputs ["graphite-clay paste"],
                                                                 :outputs [{:item-id "extruded graphite rods",
                                                                            :quantity {:units "extruded graphite core", :value-string "100000"}}],
                                                                 :resources ["extruder"],
                                                                 :duration  {:units "minutes", :value-string "20"}
                                                                 :subprocesses []},

                                                                {:process-id "dry-and-bake-core",
                                                                 :inputs ["extruded graphite rods"],
                                                                 :outputs [{:item-id "extruded graphite rods",
                                                                            :quantity {:units "extruded graphite core", :value-string "100000"}}],
                                                                 :resources ["kiln"],
                                                                 :duration  {:units "hours", :value-string "2"}
                                                                 :subprocesses []}]}

                                                {:process-id "wood-casing-production",
                                                 :inputs ["cedar wood"],
                                                 :outputs ["wood slats with grooves"],
                                                 :resources ["milling machine"],
                                                 :subprocess-flow {:val "individuals-from-batch",
                                                                   :comment (str "The string 'individuals-from-batch' means that it isn't necessary to wait for all the slats to be created;\n"
                                                                                 "you can start 'cut-grooves-in-slats' as soon as the first slat is available.")}
                                                 :duration  {:val  {:units "hours", :value-string "2"} ; ToDo: Review this comment. Improve it.
                                                             :comment "Because 'individuals-from-batch', this process's duration is (roughly speaking) the same as maximum of the two subprocesses."}
                                                 :subprocesses [{:process-id "mill-wood-slats",
                                                                 :inputs ["cedar wood"],
                                                                 :outputs ["milled wood slats"],
                                                                 :resources ["milling machine"],
                                                                 :duration  {:units "hours", :value-string "2"}
                                                                 :subprocess-flow {:val :individuals-from-batch,
                                                                                   :comment (str "'sub-process-flow' is about whether a batch must move through production steps as a batch or, alternatively, individuals from the batch can move.\n"
                                                                                                 "The string value 'individuals-from-batch' here means that it isn't necessary to wait for all the slats to be created, the process 'cut-grooves-in-slats'\n"
                                                                                                 "can start as soon as the first slat is available.")}
                                                                 :subprocesses []},

                                                                {:process-id "cut-grooves-in-slats",
                                                                 :inputs ["milled wood slats"],
                                                                 :outputs ["wood slats with grooves"],
                                                                 :resources ["groove cutter"],
                                                                 :duration  {:units "hours", :value-string "2"}
                                                                 :subprocesses []}]},

                                                {:process-id "assembly",
                                                 :inputs  {:val [{:item-id "graphite core", :from "graphite-core-production"},
                                                                 {:item-id "wood slats with grooves", :from "wood-casing-production"}
                                                                 "metal", "erasers", "paint"]
                                                           :comment (str "The 'from' property names a process that must occur before a process that uses it as an input (e.g. this 'assembly' process).\n"
                                                                         "The 'from' property is essential to understanding process ordering and potential for concurrency.")}
                                                 :outputs ["finished pencil"],
                                                 :resources ["glue applicator", "shaping machine"],
                                                 :subprocesses [{:process-id "insert-core-into-slats",
                                                                 :inputs ["graphite core", "wood slats with grooves"],
                                                                 :outputs ["pencil blanks"],
                                                                 :resources ["glue applicator"],
                                                                 :subprocesses []},

                                                                {:process-id "shape-and-paint-pencil",
                                                                 :inputs ["pencil blanks", "paint"],
                                                                 :outputs ["shaped and painted pencils"],
                                                                 :resources ["shaping machine", "painting station"],
                                                                 :subprocesses []},

                                                                {:process-id "attach-eraser",
                                                                 :optional?  {:val true,
                                                                              :comment "'optional?' means that the process does not occur for every product. Not every pencil has an eraser."}
                                                                 :inputs ["shaped and painted pencils", "metal", "erasers"],
                                                                 :outputs ["finished pencils"],
                                                                 :resources ["crimping tool"],
                                                                 :subprocesses []}]}]}]}}})

(if (s/valid? :job-shop-c/EADS-message job-shop-c)
  (let [db-obj {:EADS/id :process/job-shop--classifiable
                :EADS/cid :process
                :EADS/specs #:spec{:full :job-shop-c/EADS-message}
                :EADS/msg-str (str job-shop-c)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]})
    ;; Write the EADS JSON to resources/EADS/process so it can be placed in ork's vector store.
    (->> job-shop-c clj2json-pretty (spit "resources/EADS/process/job-shop--classifiable.json")))
  (throw (ex-info "Invalid EADS message (job-shop-c)." {})))
