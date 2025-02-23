(ns scheduling-tbd.interviewing.annotated-data-structures.flow-shop
  "(1) Define the an example annotated data structure to provide to the interviewer for a flow-shop scheduling problem.
       As the case is with flow-shop problems, this structure defines the flow of work through resources.
   (2) Define well-formedness constraints for this structure. These can also be used to check the structures produced by the interviewer."
  (:require
   [clojure.spec.alpha         :as s]
   [clojure.pprint             :refer [pprint]]
   [clojure.string             :as str]
   [jsonista.core              :as json]))

(s/def ::data-structure (s/keys :req-un [::notes-on-data-structure ::annotated-data-structure]))
(s/def ::notes-on-data-structure string?)
(s/def ::annotated-data-structure (s/keys :req-un [::process-id ::inputs ::outputs ::resources ::duration ::subprocesses]))
(s/def ::process-id ::annotated-string)
(s/def ::annotated-string (s/or string? ::string-annotation))
(s/def ::string-annotation (s/keys ::string-val
(s/def ::inputs (s/coll-of (s/or ::simple-thing ::quantified-thing)))

(def flow-ds
  "A pprinted (JSON?) version of this is what we'll provide to the interviewer at the start of Phase 2 of a flow-shop problem."
  {:notes-on-data-structure
   (str "Below is an annotated data structure that provides an example of what we'd like you to produce.\n"
        "By 'annotated' we mean that in some places, rather than just providing a primitive value (a string or number), we provide an object that contains that value (property 'val') and a comment (property 'comment').\n"
        "The comments are there to help you understand the intent of the example. You do not have to put comments in what you produce, but if you could if you'd like.\n"
        "You can use annotations anywhere. Where you choose to use them should be entirely independent of where we used them.\n"
        "Where you do use annotations, the 'comment' text should flag something about how you arrived at the 'val', for example, how it is unclear from the interviewees' answer what belongs in the value.\n"
        "For example, if you were asking the interviewees how long shipping takes, they might answer 'it depends'.\n"
        "You could flag this difficulty with an annotation: {'val' : 'it depends', 'comment' : 'The interviewees did not elaborate.'}.\n"
        "By using an annotation here, you've flagged something that we can pursue with another agent and the interviewees.")

   :annotated-data-structure
   {:process-id {:val "pencil-manufacturing",
                 :comment "This is the top-level process. You can name it as you see fit; don't ask the interviewees."}

    :inputs [{:val ["graphite", "clay", "water", "cedar wood", "metal", "eraser material", "paint"],
              :comment "These are all the raw materials used to make the product. You can figure this out by looking at all the raw materials in the leaf processes."}]

    :outputs [{:val {:item-id "finished pencils",                                                       ;/* outputs can be objects like these or strings, depending on the conversation. */
                     :quantity {:units "finished pencils" :value-string "100000"}}
               :comment (str "inputs and outputs can either be simple strings like we used above, 'graphite', clay..., or objects like this, with an 'item-id' and 'quantity'.\n"
                             "Use disgression (mindful of the questioning budget) about where you ask for quantities. Start simple and pursue details were the budget allows.")}]

    :resources [{:val ["extruder", "kiln", "milling machine", "glue applicator", "shaping machine"],    ;/* Resources, unlike inputs, are durable, and reusable. */
                 :comment "Resources, unlike inputs, are durable and reusable. Do not ask about quantities of resources; that's a conversation for another interviewer."}],

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
                    :inputs [{:val {:item-id "graphite core", :from "graphite-core-production"},
                              :comment (str "The 'from' property names a process that must occur before a process that uses it as an input (e.g. this 'assembly' process).\n"
                                            "The 'from' property is essential to understanding process ordering and potential for concurrency.")}
                             {:item-id "wood slats with grooves", :from "wood-casing-production"}
                             "metal", "erasers", "paint"],
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
                                                 :comment "'optional?' means that the process does not occur for every product.  Not every pencil has an eraser."}
                                    :process-var "attachErasers",
                                    :inputs ["shaped and painted pencils", "metal", "erasers"],
                                    :outputs ["finished pencils"],
                                    :resources ["crimping tool"],
                                    :subprocesses []}]}]}}
