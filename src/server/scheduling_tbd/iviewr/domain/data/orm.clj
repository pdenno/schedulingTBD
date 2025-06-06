(ns scheduling-tbd.iviewr.domain.data.orm
  "This provides EADS for an interview about data interviewees use in performing their work, the fundamental characteristics of those data.
   The interview creates example data from ORM fact types it formulates rather than upload their spreadsheets and talking about them.
   We work this way because we know that many people prioritize visual appeal over logical organization when using tools like Excel.
   Therefore, our plan is to use data to demonstrate the operation of the scheduling systems first, and worry about mapping their
   (possibly messy and illogical) spreadsheets into the scheduling systems later."
  (:require
   [clojure.pprint                   :refer [pprint]]
   [clojure.spec.alpha               :as s]
   [mount.core                       :as mount :refer [defstate]]
   [scheduling-tbd.agent-db          :refer [agent-log]]
   [scheduling-tbd.db                :as db]
   [scheduling-tbd.sutil             :as sutil]
   [scheduling-tbd.iviewr.eads-util  :as eads-util :refer [ds-complete?]]
   [taoensso.telemere                :as tel :refer [log!]]))

(def ^:diag diag (atom nil))

(s/def :orm/EADS-message (s/keys :req-un [::message-type ::interview-objective ::interviewer-agent ::EADS]))
(s/def ::EADS (s/keys :req-un [::EADS-id ::exhausted? ::inquiry-areas ::areas-we-intend-to-discuss]))
(s/def ::EADS-id #(= % :data/orm))
(s/def ::message-type #(= % :EADS-INSTRUCTIONS))
(s/def ::interview-objective string?)
(s/def ::interviewer-agent #(= % :data))

(s/def ::comment string?) ; About annotations

;;; (dutil/make-specs datab/orm "orm")
;;; Created Wed May 21 11:09:18 EDT 2025 using develop.dutil/make-spec.
(s/def ::exhausted? (s/or :normal :exhausted?/val :annotated ::annotated-exhausted?))
(s/def :exhausted?/val boolean?)
(s/def ::annotated-exhausted? (s/keys :req-un [::comment :exhausted?/val]))
(s/def ::areas-we-intend-to-discuss (s/or :normal :areas-we-intend-to-discuss/val :annotated ::annotated-areas-we-intend-to-discuss))
(s/def :areas-we-intend-to-discuss/val (s/coll-of string? :kind vector?))
(s/def ::annotated-areas-we-intend-to-discuss (s/keys :req-un [::comment :areas-we-intend-to-discuss/val]))
(s/def ::inquiry-areas (s/or :normal :inquiry-areas/val :annotated ::annotated-inquiry-areas))
(s/def :inquiry-areas/val (s/coll-of ::inquiry-area :kind vector?))
(s/def ::inquiry-area (s/keys :req-un [::fact-types ::inquiry-area-objects ::inquiry-area-ref]))
(s/def ::annotated-inquiry-areas (s/keys :req-un [::comment :inquiry-areas/val]))
(s/def ::fact-types (s/or :normal :fact-types/val :annotated ::annotated-fact-types))
(s/def :fact-types/val (s/coll-of ::fact-type :kind vector?))
(s/def ::fact-type (s/keys :req-un [::fact-type-id ::objects ::reference-modes ::uniqueness ::examples ::arity] :opt-un [::deontic-keys]))
(s/def ::annotated-fact-types (s/keys :req-un [::comment :fact-types/val]))
(s/def ::inquiry-area-objects (s/or :normal :inquiry-area-objects/val :annotated ::annotated-inquiry-area-objects))
(s/def :inquiry-area-objects/val (s/coll-of ::inquiry-area-object :kind vector?))
(s/def ::inquiry-area-object (s/keys :req-un [::definition ::object-id]))
(s/def ::annotated-inquiry-area-objects (s/keys :req-un [::comment :inquiry-area-objects/val]))
(s/def ::inquiry-area-ref (s/or :normal :inquiry-area-ref/val :annotated ::annotated-inquiry-area-ref))
(s/def :inquiry-area-ref/val string?)
(s/def ::annotated-inquiry-area-ref (s/keys :req-un [::comment :inquiry-area-ref/val]))
(s/def ::arity (s/or :normal :arity/val :annotated ::annotated-arity))
(s/def :arity/val number?)
(s/def ::annotated-arity (s/keys :req-un [::comment :arity/val]))
(s/def ::deontic-keys (s/or :normal :deontic-keys/val :annotated ::annotated-deontic-keys))
(s/def :deontic-keys/val (s/coll-of ::deontic-key :kind vector?))
(s/def ::deontic-key string?)
(s/def ::annotated-deontic-keys (s/keys :req-un [::comment :deontic-keys/val]))
(s/def ::examples (s/or :normal :examples/val :annotated ::annotated-examples))
(s/def :examples/val (s/keys :req-un [::column-headings ::rows]))
(s/def ::annotated-examples (s/keys :req-un [::comment :examples/val]))
(s/def ::fact-type-id (s/or :normal :fact-type-id/val :annotated ::annotated-fact-type-id))
(s/def :fact-type-id/val string?)
(s/def ::annotated-fact-type-id (s/keys :req-un [::comment :fact-type-id/val]))
(s/def ::objects (s/or :normal :objects/val :annotated ::annotated-objects))
(s/def :objects/val (s/coll-of ::object :kind vector?))
(s/def ::object string?)
(s/def ::annotated-objects (s/keys :req-un [::comment :objects/val]))
(s/def ::reference-modes (s/or :normal :reference-modes/val :annotated ::annotated-reference-modes))
(s/def :reference-modes/val (s/coll-of ::reference-mode :kind vector?))
(s/def ::reference-mode string?)
(s/def ::annotated-reference-modes (s/keys :req-un [::comment :reference-modes/val]))
(s/def ::uniqueness (s/or :normal :uniqueness/val :annotated ::annotated-uniqueness))
(s/def :uniqueness/val (s/coll-of ::uniquenes :kind vector?)) ; Odd name, but we'll run with it!
(s/def ::uniquenes (s/coll-of string? :kind vector?)) ; We wrote this one by hand (vector of vectors)
(s/def ::annotated-uniqueness (s/keys :req-un [::comment :uniqueness/val]))
(s/def ::definition (s/or :normal :definition/val :annotated ::annotated-definition))
(s/def :definition/val string?)
(s/def ::annotated-definition (s/keys :req-un [::comment :definition/val]))
(s/def ::object-id (s/or :normal :object-id/val :annotated ::annotated-object-id))
(s/def :object-id/val string?)
(s/def ::annotated-object-id (s/keys :req-un [::comment :object-id/val]))
(s/def ::column-headings (s/or :normal :column-headings/val :annotated ::annotated-column-headings))
(s/def :column-headings/val (s/coll-of ::column-heading :kind vector?))
(s/def ::column-heading string?)
(s/def ::annotated-column-headings (s/keys :req-un [::comment :column-headings/val]))
(s/def ::rows (s/or :normal :rows/val :annotated ::annotated-rows))
(s/def :rows/val (s/coll-of ::row :kind vector?))
(s/def ::row (s/coll-of string? :kind vector?)) ; We wrote this one by hand (vector of vectors).
(s/def ::annotated-rows (s/keys :req-un [::comment :rows/val]))

(def orm
    {:message-type :EADS-INSTRUCTIONS
     :interviewer-agent :data
     :interview-objective
     (str "The data interview is about discovering and documenting the information that interviewees use to do their work (scheduling production or providing a service to customers).\n"
          "Many small manufacturers use spreadsheets to run their company.\n"
          "It would seem expeditious, therefore, to have the interviewees upload their spreadsheets for discussion; but we are not going to do that.\n"
          "Instead, we are going to learn characteristics of their data through your interview and create example data of the sorts that they described to you.\n"
          "We will use these data to run prototypes of the MiniZinc-based scheduling system that the humans and AI create together.\n"
          "\n"
          "There are three principal tasks to be achieved in this interview:\n"
          "   Task 1: determining the kinds and scope of data involved in decision making in the interviewees' work,\n"
          "   Task 2a: determining the relationships and constraints among the data and expressing these as Object Role Modeling (ORM) fact types, and,\n"
          "   Task 2b: for each ORM fact type, creating a table of example data that the interviewees would deem realistic.\n"
          "\n"
          "Notice that we used the terminology 'Task 2a' and 'Task 2b' rather than 'Task 2' and 'Task 3'.\n"
          "This is to emphasize that you should focus on completing the specification of each ORM fact (Task 2a) and the production of example data (Task 2b) before addressing the next fact type.\n"
          "Working this way, you will help keep the interviewees focused.\n"
          "\n"
          "In Task 1, the goal is to categorize their quantitative and structured data into 'areas of inquiry'.\n"
          "We provide an enumeration of areas of inquiry in the EADS.\n"
          "You are encouraged to use this enumeration, but, as always, you can use EADS annotations to add categories or impart nuance using what you learn from the interviewees.\n"
          "\n"
          "In Task 2a, we are particularly interested in capturing domain semantics of the data in the viewpoint of Object Role Modeling (ORM).\n"
          "Specifically, Task 2a is about defining all the ORM fact types of the subject area of inquiry.\n"
          "The best way to do this might be to first elicit from the interviewees all the concepts (ORM objects) relevant to the area of inquiry and then suggest to them (as verbalization of a \n"
          "hypothesized fact types) how the concepts interrelate.\n"
          "For example, if interviewees have indicated that they maintain records of employee skills and skill certification dates, you might ask:\n"
          "'As you have pointed out, in your business employees have an employee number. Do you similarly use a code of some sort to describe the skill?'\n"
          "Also you might ask: 'For each employee (employee number) and skill (skill code) do you keep every certification date, or just the most recent?\n"
          "Then before initiating discussion of another fact type, do Task 2b on this one; suggest a table:\n"
          "\n"
          "'Does the following table of employee skill certification capture the sorts of information we have discussed? Feel free to edit the table.'\n"
          "#+begin_src HTML\n"
          "<table>\n"
          "   <tr><th>Employee No.</th>        <th>Skill</th>               <th>Certification Date</th></tr>\n"
          "   <tr><td>EN-123</td>              <td>Milling Centers</td>     <td>  2024-10-05           </tr>\n"
          "   <tr><td>EN-098</td>              <td>Milling Centers</td>     <td>  2022-11-13           </tr>\n"
          "   <tr><td>EN-891</td>              <td>EDM machines</td>        <td>  2023-03-28           </tr>\n"
          "</table>\n"
          "#+end_src\n"
          "As the example suggests, recall that you can include an HTML table in a question to the interviewees by wrapping the table in #+begin_src HTML ***your table*** #+end_src.\n"
          "We are able to read the tables you provide into a UI component that allows interviewees to edit the content of cells, and add and remove rows.\n"
          "\n"
          "ORM allows expression of constraints typical of a predicate calculus representation, including quantification, subtyping, cardinality, functional relationship, domain of roles, and disjointedness.\n"
          "As you will see in the EADS below, our encoding of ORM fact types borrows from ORM's visual depiction.\n"
          "For example, for an n-ary fact type, we use arrays of n elements to associate property values matching each of the n compartments of the visual depiction of the fact type role box.\n"
          "Consider, for example, the ternary fact type 'ACADEMIC obtained DEGREE from UNIVERSITY' in Figure 9 of 'Object-Role Modeling: an overview' (a paper provided to you).\n"
          "We would encode this fact type as:\n"
          "\n"
          (clj2json-pretty
           {:fact-type-id "ACADEMIC-obtains-DEGREE-from-UNIVERSITY"
            :arity 3
            :objects ["academic" "degree" "university"]
            :reference-modes  ["empNr" "code" "code"]
            :deontic-keys ["mandatory" "" ""]
            :uniqueness [["key1" "key1" ""]]})
          "\n"
          "Here the three object properties 'objects', 'reference-modes', and 'deontic' must each contain three elements because that is the arity of the fact type.\n"
          "The three positions correspond to the 'academic', 'degree' and 'university' compartments  respectively. (This is the order of the 'objects' property.)\n"
          "There can be multiple ORM 'uniqueness' constraints on a fact type; each array valued element must likewise contain the same number of elements as the arity and same ordering as the 'objects' property.\n"
          "In the above " (clj2json-pretty ["key1" "key1" ""]) " represents the idea that there is a functional relationship between tuples [academic, degree], as domain and univerity, as co-domain.\n"
          "Were we to live in a world where people can get at most one degree at any university, we could specify another ORM uniqueness constraint " (clj2json-pretty ["key2" "" "key2"]) " which maps\n"
          "tuples [academic, university] to a degree."
          "\n"
          "ORM also has provision to express constraints across fact types, and between object types.\n"
          "Figure 9 of the paper depicts that (1) an academic being tenured and being contracted are exclusive of each other, and (2) professor is a kind of academic.\n"
          "We represent these two constraints with the two following objects respectively:\n"
          "\n"
          (clj2json-pretty
           {:inter-fact-type-id "tenured-or-contracted"
            :relation-type "exclusive-or"
            :fact-type-roles [{:fact-type-ref "ACADEMIC-is-tenured"
                               :role-position 1}
                              {:fact-type-ref "ACADEMIC-is-contracted-till"
                               :role-position 1}]})
          "and\n"
          (clj2json-pretty
           {:inter-object-id "PROFESSOR-is-ACADEMIC"
            :relation-type "is-kind-of"
            :source-object "professor"
            :target-object "academic"})
          "\n"
          "\n"
          "SUMMARY RECOMMENDATIONS\n"
          "We encourage you to start the interview (start Task 1) with an open-ended question about the kinds of data the interviewees use, for example, for interviewees involved in manufacturing you might ask:\n"
          "\n"
          "   'To get started, could you list the kinds of data that you use to schedule production?\n"
          "    For example, do you have speadsheets containing customer orders, raw material delivery, process plans, materials on hand, task durations, worker skills, etc.?\n"
          "\n"
          "Given the response from this, you can set the 'areas-we-intend-to-discuss' property to a list of strings naming what areas the interviewees' response suggest are important to discuss.\n"
          "\n"
          "You can then discuss each area of inquiry they mention (repeating Task 2a and Task 2b for each fact type of the area of inquiry) in whatever order you deem appropriate."
          "When you have completed the detailed discussion of every area of inquiry they have thus far mentioned, you should ask them whether there is yet more areas to discuss.\n"
          "If they mention more, continue to apply the three tasks until all areas are discussed.\n"
          "The EADS has a property, 'exhausted?', you should set to true when you believe this interview can be concluded.\n"
          "\n"
          "You have a choice as to how you communicate back to us in DATA-STRUCTURE-REFINEMENT (DSR) messages: you can accummulate results from several inquiry areas into one ever-growing DSR message, or\n"
          "(probably easier for you) you can limit what is a DSR message to just one or a few inquiry areas (thue one or a few elements in the :areas-we-intend-to-discuss.\n"
          "Just be sure to only set 'exhausted?' to true when you are finished with the last inquiry that you planned to discuss.\n"
          "\n"
          "ORM is designed to encourage verbalization of fact types.\n"
          "We encourage you to use such verbalizations in Task 2a as follow-up questions when the interviewees' response leaves you uncertain what fact type is intended.\n"
          "For example, in Task 2a you might have discussed a fact type corresponding to the table above with rows 'Employee No.', 'Skill', and 'Certification Date' as described above.\n"
          "But it was unclear whether or not they were keeping a history of certification dates or just a single date. In this case you might ask:\n"
          "'Is it the case that you associate at most one Certification Date with each employee and skill?'\n"
          "\n"
          "The interview you conduct may prove to be rather complex and possibly long-running, but it is very important to our work, so we are giving you a big budget for question asking.\n"
          "Good luck!")
     :EADS
     {:EADS-id :data/orm
      :exhausted? {:val false
                   :comment "You don't need to specify this property until you are ready to set its value to true, signifying that you believe that all areas of inquiry have been sufficiently investigated.\n"}
      :areas-we-intend-to-discuss ["customer-orders", "workforce"]
      :inquiry-areas
      [{:inquiry-area-ref {:val "customer-orders"
                           :comment (str "'customer-orders' is a value in an enumeration of areas of inquiry. The enumeration values are defined as follows:\n"
                                         "\n"
                                         "'customer-orders' - about products customers are ordering, their quantities, due dates, expected ship dates, etc..\n"
                                         "'customers' - about the customers themselves, shipping address, their standing in the company's preferred customer program, etc. This could be quite diverse!\n"
                                         "'materials' - about things that go into making products, including things on hand, en route to the facility, or on order, their expected delivery dates, etc..\n"
                                         "'bill-of-materials' - about what materials go into creating a product of a given product type.\n"
                                         "'finished-goods' - about finished goods inventory.\n"
                                         "'WIP' - about work in process, its state of completion etc.\n"
                                         "'processes' - about production processes and process plans, tasks, task durations, equipment used, etc..\n"
                                         "'facilities' - about places where they make product or perform services, and what equipment is present in these places.\n"
                                         "'equipment' - about machines and tools, their capabilities, capacity, and number.\n"
                                         "'workforce' - about people, their skills, and other information about them relevant to scheduling.\n"
                                         "'holidays' - about holidays and planned periods of plant shut down.\n"
                                         "\n"
                                         "This enumeration might be incomplete. Whenever nothing here seems to fit, create another term and define it with an annotation comment."
                                         "\n"
                                         "When Task 1 is completed but you have not yet started Task 2a on any fact types, the 'inquiry-areas' property will contain a list of simple objects such as "
                                         (clj2json-pretty {:inquiry-area-ref "customer-orders"}) " " (clj2json-pretty {:inquiry-area-ref "WIP"}) " and so on.\n\n"
                                         "It is important to keep in mind that we are developing the scheduling system incrementally.\n"
                                         "The first few versions of it might be rudimentary and incomplete. This is intentional! We want to show the humans how MiniZinc works using these rudimentary versions.\n"
                                         "How this affects your work is that, unless you have been specifically told otherwise, you should limit discussion to areas of inquiry that you think are essential to\n"
                                         "creating the simplest complete scheduling system. Therefore, unless you have been specifically told otherwise, you should not encourage discussion of WIP, finished-goods,\n"
                                         "facilities, and holidays (and others that might come to mind to you or the interviewees). But this is just a rule-of-thumb; use your best judgment.")}

        :inquiry-area-objects
        {:comment (str "This property provides a list of objects (in the JSON sense) where each object names an object in the ORM sense (entities) and provides a definition for it.\n"
                       "These represent the relevant entities of the universe of discourse of the area of inquiry.")
         :val [{:object-id "product"
                :definition {:comment (str "You don't have to ask the interviewees for a definition; if what is intended seems obvious just provide that.\n"
                                           "Object-ids need only be unique within the context of an area of inquiry.")
                             :val "a unique identifier for the product type."}}
               {:object-id "order"
                :definition "a string unique to their operations for identifying an order."}
               {:object-id "customer"
                :definition "the person or organization for which the product is being provided."}
               {:object-id "promise-date"
                :definition "The date by which the firm promised to have delivered the product to the customer."}
               {:object-id "quantity"
                :definition "An amount of something. (In the narrow context being defined, the quantity of product ordered."}]}

        :fact-types
        {:comment "This property provides a list of ORM fact type objects involving the inquiry-area-objects. Thus this captures actual Task 2 ORM modeling."
         :val [{:fact-type-id "ORDER-has-PROMISE-DATE"
                :arity 2,
                :objects ["order" "promise-date"]
                :reference-modes ["order-number" "timepoint"]
                :deontic-keys ["mandatory" ""]
                :uniqueness {:val [["key1" ""]]
                             :comment (str "Since every order participates in this relationship (mandatory), and order, through the order-number, uniquely identifies a promise date (uniqueness),\n"
                                           "we can infer that every order is associated with exactly one promise date.")}
                :examples {:comment "Completing this is the work of Task 2b. We are showing only three rows of data in this example. Typically you might show ten or so."
                           :val {:column-headings {:val ["order-number" "promise-date"]
                                                   :comment (str "The interviewer (you) used the reference-mode 'order-number' but the object name 'promise-date'.\n"
                                                                 "This is the most natural and meaningful naming for these data.")}
                                 :rows [["CO-865204" "2025-11-06"]
                                        ["CO-863393" "2025-11-13"]
                                        ["CO-865534" "2025-03-28"]]}}}
               {:fact-type-id "ORDER-has-PRODUCT-QUANTITY"
                :arity 3,
                :objects ["order" "product" "quantity"]
                :reference-modes ["order-number" "product-code" "quantity"]
                :deontic-keys ["mandatory" "" ""]
                :uniqueness [["key1" "key1" ""]]
                :examples {:column-headings ["order-number" "product-code" "quantity"]
                           :rows [["CO-865204" "PN-38553" "1 unit"]
                                  ["CO-863393" "PN-37454" "7 unit"]
                                  ["CO-865534" "PN-73853" "2 family pack"]]}}
               {:fact-type-id "ORDER-is-for-CUSTOMER"
                :arity 2,
                :objects ["order" "customer"]
                :reference-modes ["order-number" "customer-id"]
                :deontic-keys ["mandatory" ""]
                :uniqueness [["key1" ""]]
                :examples {:column-headings ["order-number" "customer-id"]
                           :rows [["CO-865204" "CID-8811"]
                                  ["CO-863393" "CID-8955"]
                                  ["CO-865534" "CID-0013"]]}}]}}

       {:inquiry-area-ref "workforce"
        :inquiry-area-objects [{:object-id "employee"
                                :definition "someone who works for the company."}
                               {:object-id "skill"
                                :definition "a capability of an employee described by a skill code"}
                               {:object-id "certification"
                                :definition "the passing of a test about ones ability at a specific task."}]
        :fact-types [{:fact-type-id "EMPLOYEE-certifies-SKILL-at-DATE"
                      :arity 3,
                      :objects ["employee" "skill" "certification"]
                      :reference-modes {:val ["employee-number" "skill-code" "timepoint"]
                                        :comment (str "Regarding the 'timepoint' reference mode,  the interviewees use 'certification' and 'certification-date' interchangeably.\n"
                                                      "Similarly, we conflate the concept with the time of the event.")}
                      :uniqueness [["key1" "key1" ""]]
                      :examples {:column-headings ["Employee No." "Skill"  "Certification Date"]
                                 :rows [["EN-123" "Milling Centers" "2024-10-05"]
                                        ["EN-098" "Milling Centers" "2022-11-13"]
                                        ["EN-891" "EDM machines"    "2023-03-28"]]}}]}]}})

;;; ------------------------------- checking for completeness ---------------
;;; (-> orm/orm :EADS eads-util/strip-annotations orm/completeness-test)
(defn completeness-test
  "The ORM ds is complete when the interviewer sets :exhausted to true.
   We trust the interviewer on this one."
  [eads]
  (:exhausted? eads))

(defn ds-refine2ds
  "Translate the argument DATA-STRUCTURE-REFINEMENT message to EADS."
  [refine-msg]
  (as-> refine-msg ?m
    (:data-structure ?m)
    (eads-util/strip-annotations ?m)
    (if (contains? ?m :EADS-ref)
      (assoc :EADS-id (:EADS-ref refine-msg))
      ?m)
    (dissoc ?m :EADS-ref)))

(defmethod ds-complete? :data/orm
  [refine-msg]
  (let [ds (ds-refine2ds refine-msg)
        complete? (completeness-test ds)]
    (agent-log (str "This is the stripped DS for ORM (complete? = " complete? "):\n" (with-out-str (pprint ds)))
               {:console? true :elide-console 130})
    (when-not (s/valid? ::EADS ds)
      (reset! diag orm)
      (agent-log (str "EADS is invalid:\n" (with-out-str (pprint ds)))
                 {:console? true :level :warn :elide-console 130}))
    complete?))

;;; -------------------- Starting and stopping -------------------------
(defn init-orm
  []
  (if (s/valid? :orm/EADS-message orm)
    (when-not (db/same-EADS-instructions? orm)
      (log! :info "Updating orm in resources and system DB.") ; You won't see this!
      (sutil/update-resources-EADS-json! orm)
      (db/put-EADS-instructions! orm))
    (throw (ex-info "Invalid EADS message (orm)." {}))))

(defstate orm-eads
  :start (init-orm))
