(ns scheduling-tbd.interviewing.domain.process.flow-shop
  "(1) Define the example annotated data structure (EADS) interviewer will use for questioning about a flow-shop scheduling problem.
       As the case is with flow-shop problems, this structure defines the flow of work through resources.
   (2) Define well-formedness constraints for this structure. These can also be used to check the structures produced by the interviewer."
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))

;;; ToDo: Consider replacing spec with Malli, https://github.com/metosin/malli .
;;; ToDo: Someday it might make sense to have an agent with strict response format following these specs.

(def ^:diag diag (atom nil))

(s/def :flow-shop/EADS-message (s/keys :req-un [::message-type ::interview-objective ::interviewer-agent ::EADS]))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent #(= % :process))

(s/def ::comment string?) ; About annotations

(s/def ::EADS (s/keys :req-un [::EADS-id ::process-id ::inputs ::outputs ::resources ::subprocesses] :opt-un [::duration]))
(s/def :flow-shop/graph (s/keys :req-un [::inputs ::outputs ::resources ::subprocesses] :opt-un [::duration]))
(s/def ::EADS-id #(= % :process/flow-shop))

;;; We use the 'trick' that :<some-property>/val can be used that to signify a non-namespaced attribute 'val' and a reference to a spec for value of 'val'.
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

;;; ToDo: Write something about flow-shops being disjoint from the other four types. But also point out that part of their complete process could be flow shop....
;;;       Come up with an example where work flows to a single-machine-scheduling problem.

;;; (s/valid? ::fshop/EADS (:EADS fshop/flow-shop))
(def flow-shop
  "A pprinted (JSON?) version of this is what we'll provide to the interviewer at the start of a flow-shop problem.
   Like all EADS, it is also stored in the system DB. See how at the bottom of this file.
   We use the data structure created by this EADS to create something like functional flow block diagrams (FFBDs).
   In our notation, we infer concurrency by looking at relationships between inputs of one process and output of another.
   For the time being however, if the adjacent processes in the :subprocesses vector have the same inputs and outputs, we assume the are sequential.
   We aren't sure this makes sense, and it is likely that there are shortcomings in using matching on inputs and outputs to decide concurrency/sequentiality.
   For the time being we'll live with it. Maybe the ability to edit charts with help from an AI agent can be used to fix bugs."
  {:message-type :EADS-INSTRUCTIONS
   :interviewer-agent :process
   :interview-objective (str "This EADS assumes the interviewees' production processes are organized as a flow shop.\n"
                             "Learn about the interviewees' production processes, their interrelation, inputs, outputs, and duration.\n"
                             "We might learn through further discussion that they actually don't want to develop a scheduling system to schedule the flow-shop\n"
                             "For example, they might have in mind scheduling machine maintenance, not production.\n"
                             "This fact would not prevent us from pursuing knowledge of how the make product or deliver the service that is revealed through this interview.\n"
                             "Knowledge of the processes might prove useful later.")
   :EADS
   {:EADS-id :process/flow-shop
    :process-id {:val "pencil-manufacturing",
                 :comment "This is the top-level process. You can name it as you see fit; don't ask the interviewees."}

    :inputs {:val ["graphite", "clay", "water", "cedar wood", "metal", "eraser material", "paint"],
              :comment "These are all the raw materials used to make the product. It is a collection of all the raw materials in subprocesses."}

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
                    :outputs [{:item-id "finished graphite rods"
                               :quantity {:units "graphite cores" :value-string "100000"}}],
                    :resources ["mixer", "extruder", "kiln"],
                    :subprocesses [{:process-id "mix-graphite-and-clay",
                                    :inputs ["graphite", "clay", "water"],
                                    :outputs [{:item-id "graphite clay paste",
                                               :quantity {:units "liters", :value-string "100"}}],
                                    :resources ["mixer"],
                                    :duration  {:units "hours", :value-string "1"}
                                    :subprocesses {:val []
                                                   :comment (str "We use empty array val values to signify that we don't think there are any interesting subprocess from the standpoint of scheduling.\n"
                                                                 "Of course, this could be updated later if subsequent discussion suggests we are wrong.")}}

                                   {:process-id "extrude-core",
                                    :inputs ["graphite clay paste"],
                                    :outputs [{:item-id "extruded graphite rods",
                                               :quantity {:units "extruded graphite core", :value-string "100000"}}],
                                    :resources ["extruder"],
                                    :duration  {:units "minutes", :value-string "20"}
                                    :subprocesses []},

                                   {:process-id "dry-and-bake-core",
                                    :inputs ["extruded graphite rods"],
                                    :outputs [{:item-id "finished graphite rods",
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
                                                      :comment (str "'subprocess-flow' is about whether a batch must move through production steps as a batch or, alternatively, individuals from the batch can move.\n"
                                                                    "The string value 'individuals-from-batch' here means that it isn't necessary to wait for all the slats to be created, the process 'cut-grooves-in-slats'\n"
                                                                    "can start as soon as the first slat is available.")}
                                    :subprocesses []},

                                   {:process-id "cut-grooves-in-slats",
                                    :inputs ["milled wood slats"],
                                    :outputs ["wood slats with grooves"],
                                    :resources ["groove cutter"],
                                    :duration  {:units "hours", :value-string "2"}
                                    :subprocesses []}]},

                   {:process-id "assemble",
                    :inputs  {:val [{:item-id "finished graphite rods", :from "graphite-core-production"},
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
                                    :subprocesses []}]}]}})

(if (s/valid? :flow-shop/EADS-message flow-shop)
  (let [db-obj {:EADS/id :process/flow-shop
                :EADS/cid :process
                :EADS/specs #:spec{:full :flow-shop/EADS-message}
                :EADS/msg-str (str flow-shop)}
        conn (connect-atm :system)
        eid (d/q '[:find ?e . :where [?e :system/name "SYSTEM"]] @conn)]
    (d/transact conn {:tx-data [{:db/id eid :system/EADS db-obj}]})
    ;; Write the EADS JSON to resources/EADS/process so it can be placed in ork's vector store.
    (->> flow-shop clj2json-pretty (spit "resources/EADS/process/flow-shop.json")))
  (throw (ex-info "Invalid EADS message (flow-shop)." {})))
