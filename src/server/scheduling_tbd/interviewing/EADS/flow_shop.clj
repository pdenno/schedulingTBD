(ns scheduling-tbd.interviewing.EADS.flow-shop
  "(1) Define the an example annotated data structure (EADS) to provide to the interviewer for a flow-shop scheduling problem.
       As the case is with flow-shop problems, this structure defines the flow of work through resources.
   (2) Define well-formedness constraints for this structure. These can also be used to check the structures produced by the interviewer.

   Note: We don't load this code at system startup. When you compile it, it writes the EADS to resources/EADS/flow-shop.txt"
  (:require
   [clojure.pprint             :refer [pprint]]
   [clojure.set]
   [clojure.spec.alpha         :as s]
   [taoensso.telemere          :as tel :refer [log!]]))

;;; ToDo: Someday it might make sense to have an agent with strict response format following these specs.

#_:clj-kondo/ignore
(s/def ::data-structure (s/keys :req-un [::notes-on-data-structure ::annotated-data-structure]))
(s/def ::EADS (s/keys :req-un [::process-id ::inputs ::outputs ::resources ::subprocesses] :opt-un [::duration]))
(s/def ::notes-on-data-structure string?)
(s/def ::process (s/keys :req-un [::process-id ::subprocesses] :opt-un [::duration ::inputs ::outputs ::resources]))
(s/def ::annotated-data-structure ::process) ; Interesting that forward reference of ::process does not work.
(s/def ::comment string?)

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

#_:clj-kondo/ignore
(s/def ::quantified-thing (s/keys :req-un [::item-id ::quantity]))
(s/def ::item-id string?)
(s/def ::quantity (s/keys :req-un [::units ::value-string]))
(s/def ::units string?)
(s/def ::value-string string?)

;;; (s/valid? ::fshop/EADS (:EADS fshop/flow-shop))
(def flow-shop
  "A pprinted (JSON?) version of this is what we'll provide to the interviewer at the start of Phase 2 of a flow-shop problem."
  {:interview-objective "Produce a data structure similar in form to the EADS in this object, but describing the interviewees' production processes."
   :EADS
   {:process-id {:val "pencil-manufacturing",
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
                                    :subprocesses []},

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
                                    :subprocesses []}]}]}})

(if (s/valid? ::EADS (:EADS flow-shop))
  (spit "resources/EADS/flow-shop.edn"
        (with-out-str
          (pprint
           {:message-type :PHASE-2-EADS
            :EADS flow-shop #_(with-out-str (clojure.data.json/pprint (:EADS flow-shop)))})))
  (do (log! :error "flow-shop EADS does not pass specs.")
      {:invalid-EADS-spec :flow-shop}))


;;; 1) Condition this object


(def example-1 {:message-type "DATA-STRUCTURE-REFINEMENT",
                :commit-notes "Incorporated production constraints such as batch sizes, lead times, equipment limitations, mandatory processing times, and material compatibility factors."
                :data-structure {:process-id "optical-fiber-manufacturing",
                                 :subprocesses
                                 [{:constraints
                                   {:mandatory-cooling-time {:val "required for annealing"},
                                    :minimum-lead-time {:comment "Precision and controlled conditions required.", :val "1 day"},
                                    :maximum-batch-size {:val "limited by reactor chamber size"}},
                                   :process-id "preform-manufacturing",
                                   :outputs ["glass preform"],
                                   :duration {:units "days", :value-string "1-7"},
                                   :inputs ["silicon tetrachloride" "germanium tetrachloride" "oxygen" "glass tubing"],
                                   :resources ["chemical vapor deposition reactors" "glass lathes" "annealing furnaces"]}
                                  {:constraints {:equipment-specific-limitation {:val "single preform at a time"}},
                                   :process-id "fiber-drawing",
                                   :outputs ["optical fiber strands"],
                                   :duration {:units "hours", :value-string "6-12"},
                                   :inputs [{:item-id "glass preform", :from "preform-manufacturing"}],
                                   :resources ["fiber drawing towers" "precision furnaces" "drawing capstans" "diameter measurement systems"]}
                                  {:constraints {:mandatory-processing-time {:val "required for curing"}},
                                   :process-id "coating",
                                   :outputs ["coated fiber"],
                                   :duration {:units "hours", :value-string "6-12"},
                                   :inputs [{:item-id "optical fiber strands", :from "fiber-drawing"}],
                                   :resources ["inline coating applicators" "UV curing systems" "coating thickness measurement tools"],
                                   :subprocess-flow "simultaneous-with-previous"}
                                  {:process-id "quality-testing",
                                   :outputs ["qualified fiber"],
                                   :duration {:units "hours", :value-string "2-4"},
                                   :inputs [{:item-id "coated fiber", :from "coating"}],
                                   :resources ["optical time-domain reflectometers" "tensile testing machines" "refractive index profilers" "attenuation measurement systems"]}
                                  {:subprocesses
                                   [{:process-id "additional-thermal-treatments",
                                     :outputs ["specialty-treated fiber"],
                                     :duration {:units "variable", :value-string "dependent on specifications"},
                                     :inputs [{:item-id "qualified fiber", :from "quality-testing"}],
                                     :optional? true,
                                     :resources ["high-temperature furnaces"]}
                                    {:process-id "custom-inspections",
                                     :outputs ["verified specialty fiber"],
                                     :duration {:units "hours", :value-string "variable"},
                                     :inputs [{:item-id "qualified fiber", :from "quality-testing"}],
                                     :optional? true,
                                     :resources ["precision measurement tools" "inspection stations"]}],
                                   :process-id "optional-steps"}
                                  {:process-id "cabling-packaging",
                                   :outputs ["packaged fiber" "assembled fiber cables"],
                                   :duration {:units "hours", :value-string "2-6"},
                                   :inputs [{:item-id "qualified fiber", :from "quality-testing"} "cable jacketing materials" "packaging supplies"],
                                   :resources ["cabling lines" "extrusion systems" "spooling machines" "specialized packaging tools"]}
                                  {:process-id "equipment-setup",
                                   :outputs ["calibrated and aligned equipment"],
                                   :duration {:units "hours", :value-string "variable"},
                                   :inputs ["operational equipment"],
                                   :resources ["setup tools" "specialized technicians"]}
                                  {:process-id "maintenance",
                                   :outputs ["maintained equipment"],
                                   :duration {:units "variable", :value-string "planned downtime"},
                                   :inputs ["functioning equipment"],
                                   :resources ["maintenance tools" "trained technicians"]}
                                  {:constraints {:material-compatibility {:val "required for switching between fibers"}},
                                   :process-id "cleaning",
                                   :outputs ["cleaned equipment"],
                                   :duration {:units "variable", :value-string "dependent on need"},
                                   :inputs ["used equipment"],
                                   :resources ["cleaning supplies" "technicians"]}
                                  {:process-id "material-procurement",
                                   :outputs ["procured materials"],
                                   :duration {:units "variable", :value-string "dependent on supply chain"},
                                   :inputs ["high-purity raw materials"],
                                   :resources ["supply chain management systems" "procurement specialists"]}
                                  {:process-id "waste-management",
                                   :outputs ["disposed waste following regulations"],
                                   :duration {:units "variable", :value-string "scheduled intervals"},
                                   :inputs ["byproducts"],
                                   :resources ["waste management systems" "environmental safety technicians"]}
                                  {:process-id "changeover-times",
                                   :outputs ["adjusted equipment"],
                                   :duration {:units "hours", :value-string "variable based on requirements"},
                                   :inputs ["production setup"],
                                   :resources ["setup technicians" "adjustment tools"]}]}})

(def example-2
  {:message-type "DATA-STRUCTURE-REFINEMENT",
    :commit-notes "Added reprocessing and recycling steps for defective glass sheets and byproducts, detailing how cullet is reintegrated and how specialty scraps are handled."
   :data-structure
   {:reprocessing-and-recycling
    [{:description
      "Defective glass sheets from annealing or cutting are crushed and recycled as cullet, re-melted with raw materials in the furnace."}
     {:description "Trimmings and waste glass from cutting are collected and recycled into the production process."}
     {:description
      "Defective laminated or coated glass cannot be recycled into the furnace and is sent to specialized recycling facilities or processed separately if possible."}],
    :subprocesses
    [{:process-id "raw-material-preparation",
      :outputs ["mixed glass batch"],
      :duration {:units "hours", :value-string "1-2"},
      :inputs ["silica sand" "soda ash" "lime" "additives"],
      :resources ["mixers" "labor"]}
     {:process-id "melting",
      :outputs ["molten glass"],
      :duration {:units "hours", :value-string "24-48"},
      :inputs ["mixed glass batch" "energy (gas/electricity)"],
      :resources ["furnace" "skilled operators"]}
     {:process-id "shaping",
      :outputs ["shaped glass sheets"],
      :duration {:units "minutes", :value-string "30-60"},
      :inputs ["molten glass"],
      :resources ["float bath/tin bath" "rollers"]}
     {:process-id "annealing",
      :outputs ["stress-relieved glass sheets"],
      :duration {:units "hours", :value-string "3-4"},
      :inputs ["shaped glass sheets"],
      :resources ["annealing lehr" "temperature control systems"]}
     {:process-id "cutting",
      :outputs ["glass cut to size"],
      :duration {:units "minutes", :value-string "30"},
      :inputs ["cooled glass sheets"],
      :resources ["cutting tools" "skilled labor"]}
     {:process-id "tempering-process",
      :outputs ["tempered (strengthened) glass"],
      :duration {:units "hours", :value-string "1-2"},
      :inputs ["cut glass sheets"],
      :resources ["heat furnace" "quenching equipment"]}
     {:process-id "laminating-process",
      :outputs ["laminated glass"],
      :duration {:units "hours", :value-string "1-5"},
      :inputs ["glass sheets" "interlayer material (e.g. PVB)"],
      :resources ["autoclave" "rollers" "lamination equipment"]}
     {:process-id "coating-applications",
      :outputs ["coated functional glass"],
      :duration {:units "minutes-hours", :value-string "30-120"},
      :inputs ["glass sheets" "coating chemicals"],
      :resources ["coating machines" "drying equipment"]}],
    :dependencies
    [{:description "Cooling times must be observed during annealing before cutting or further processing."}
     {:description "Quality control checkpoints at each step must be passed before proceeding."}
     {:description "Limited availability of furnaces, lehrs, or lamination equipment can introduce bottlenecks."}
     {:description "Melting and shaping as continuous processes require batch synchronization to minimize downtime."}
     {:description "Environmental conditions like temperature and humidity control are critical for lamination and coating."}
     {:description
      "Overlap potential exists; finishing steps like tempering or laminating can overlap with the next batch entering earlier processes depending on machine availability."}],
    :process-id "plate-glass-production",
    :external-factors
    [{:description "Timely delivery of raw materials is critical to avoid production disruptions."}
     {:description "Seasonal demand peaks during construction seasons or before weather changes require planning."}
     {:description "Energy-intensive processes are influenced by electricity or gas price fluctuations."}
     {:description "Custom or specialty orders with strict deadlines add scheduling pressure."}
     {:description "Regular maintenance or unexpected equipment breakdowns require rescheduling."}
     {:description "Regulatory compliance, such as emissions limits, may restrict production at times."}],
    :quality-control-checks
    [{:check "Inspect purity and proper mix of silica sand, soda ash, and additives.",
      :action-on-issue "Correct mix before melting.",
      :stage "Raw Material Preparation"}
     {:check "Ensure clarity and impurity levels using visual and automated checks.",
      :action-on-issue "Skim off impurities as needed.",
      :stage "Melting"}
     {:check "Monitor thickness and flatness with sensors and gauges.",
      :action-on-issue "Trigger adjustments in shaping equipment for deviations.",
      :stage "Shaping"}
     {:check "Check for internal stresses using polarization tools.",
      :action-on-issue "Recycle or discard defective sheets.",
      :stage "Annealing"}
     {:check "Ensure proper dimensions through measurements.", :action-on-issue "Discard incorrectly cut pieces.", :stage "Cutting"}
     {:check "Verify surface strength, adhesion, or coating integrity.",
      :action-on-issue "Rework or remove defective items from batch.",
      :stage "Specialty Processing (Tempering, Laminating, Coating)"}],
    :batch-and-lot-constraints
    [{:description "Minimum batch size must match the melting furnace's economic operating volume, typically several tons."}
     {:description "Maximum batch size is limited by the capacity of tanks and annealing lehrs."}
     {:description "Specialty glass products require smaller batches due to longer processing times and custom specifications."}
     {:description "Customer orders dictate lot sizes, sometimes necessitating batch splitting or combining."}
     {:description "Changeovers between product specifications impose time costs, making larger single-type lots more efficient."}]}})

(defn collect-keys [obj]
  (let [okeys (atom #{})]
    (letfn [(ck [obj]
              (cond (map? obj)      (doseq [[k v] obj]
                                      (swap! okeys conj k)
                                      (ck v))
                    (vector? obj)   (doseq [x obj] (ck x))))]
      (ck obj)
      (sort @okeys))))

(def eads-keys (-> flow-shop :EADS collect-keys set))

(defn extra-keys [obj]
  (clojure.set/difference (-> obj collect-keys set) eads-keys))
