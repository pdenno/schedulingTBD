(ns scheduling-tbd.interviewing.domain.data.data-basics
  "Planning operators for the data interview"
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.db] ;for mount
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))


(def data-basics
    {:message-type :EADS-INSTRUCTIONS
     :interviewer-agent :data
     :interview-objective
     (str "The data interview is about discovering and documenting the information that interviewees use to do their work (scheduling production or providing a service to customers).\n"
          "Many small manufacturers use spreadsheets to run their company.\n"
          "It would seem expeditious, therefore, to have the interviewees upload their spreadsheets for discussion; but we are not going to do that.\n"
          ;; (because we know that many people prioritize visual appeal over logical organization when using tools like Excel!)
          ;; We'll deal with their spreadsheets (if they are even willing to share them) once we understand their intent better.
          "Instead, we are going to try to create example data of the sorts that they described to you.\n"
          "We will use these data to run prototypes of the MiniZinc-based scheduling system that the humans and AI create together.\n"
          "\n"
          "There are three principal tasks to be achieved in this interview:\n"
          "   Task 1: determining the kinds and scope of data involved in decision making in the interviewees' work,\n"
          "   Task 2: determining the relationships and constraints among the data, and,\n"
          "   Task 3: creating examples of the data that the interviewees would deem realistic.\n"
          "\n"
          "In Task 1, the goal is to categorize quantitative and structured data into 'areas of inquiry'.\n"
          "We provide an enumeration of areas of inquiry you are encouraged to use but, as always, you can use EADS annotations as described earlier to impart nuance to what you learned from the interviewees.\n"
          "\n"
          "In Task 2, we are particularly interested in capturing domain semantics in the viewpoint of Object Role Modeling (ORM).\n"
          "ORM allows expression of the constraints typical of predicate calculus, including quantification, subtyping, cardinality, functional relationship, domain of roles, disjointedness.\n"
          "As you will see in the EADS below, our encoding of ORM fact types borrows from ORM's visual depiction.\n"
          "For example, we represent properties of the compartments of ORM's fact type role box with ordered lists containing as many elements as the arity of the predicate.\n"
          "See, for example, the ternary fact type 'ACADEMIC obtained DEGREE from UNIVERSITY' on Figure 9 of 'Object-Role Modeling: an overview' (a paper provided to you).\n"
          "We would encode this fact type as:\n"
          "\n"
          (clj2json-pretty
           {:fact-type-id "ACADEMIC-obtains-DEGREE-from-UNIVERSITY"
            :arity 3
            :objects ["academic" "degree" "university"]
            :reference-modes  ["empNr" "code" "code"]
            :deontic ["mandatory" "" ""]
            :uniqueness [["key1" "key1" ""]]})
          "\n"
          "Here the three object properties 'objects', 'reference-modes', and 'deontic' must each contain three elements because that is the arity of the fact type.\n"
          "The three positions correspond to the 'academic', 'degree' and 'university' compartments  respectively. (This is the order of the object property.)\n"
          "There can be multiple ORM uniqueness constraints on a fact type; each array valued element must likewise contain the same number of elements as the arity and ordering.\n"
          "In the above " (clj2json-pretty ["key1" "key1" ""]) " represents the idea that there is a functional relationship between tuples [academic, degree], as domain and and univerity, as co-domain.\n"
          "Were we to live in a world where people can get at most one degree at any university, we could specify another ORM uniqueness constraint " (clj2json-pretty ["key2" "" "key2"]) " which maps\n"
          "tuples [academic, university] to a degree."
          "\n"
          "ORM allows constraints across fact types, and between object types.\n"
          "Figure 9 of the paper depicts that (1) being tenured and being contracted are exclusive of each other, and (2) professor is a kind of academic we have:\n"
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
            :relation-type "kind-of"
            :from-object "professor"
            :to-object "academic"})
          "\n"
          "In Task 3, we would like you to create realistic examples of the data of each area of inquiry in tabular form.\n"
          "Recall that you can include an HTML table in questions to the interviewees by wrapping the table in #+begin_src HTML ***your table*** #+end_src.\n"
          "An example of this is below:\n"
          "\n"
          "#+begin_src HTML\n"
          "<table>\n"
          "   <tr><th>Employee No.</th>        <th>Skill</th>               <th>Certification Date</th></tr>\n"
          "   <tr><td>N123</td>                <td>Milling Center</td>      <td>  2024-10-05           </tr>\n"
          "   <tr><td>N098</td>                <td>Milling Center</td>      <td>  2022-11-13           </tr>\n"
          "   <tr><td>N891</td>                <td>EDM machines</td>        <td>  2023-03-28           </tr>\n"
          "</table>\n"
          "#+end_src\n"
          "\n"
          "We are able to read the tables you provide into a nice UI component that allows the users to edit the content of cells and add and remove rows.\n"
          "\n"
          "SUMMARY\n"
          "We encourage you to start the interview (start Task 1) with an open-ended question about the kinds of data the interviewees use, for example, you might ask:\n"
          "\n"
          "   'To get started, could you list the kinds of data that you use to schedule production?\n"
          "    For example, do you have speadsheets containing customer orders, raw material delivery, process plans, materials on hand, task durations, worker skills, etc.?\n"
          "\n"
          "You can then discuss each area of inquiry (Task 2) in whatever order you deem appropriate."
          "When you have completed the detailed discussion of every area of inquiry they have thus far mentioned, you should ask them whether there is yet more areas to discuss.\n"
          "If they mention more, continue to apply the three tasks until all areas are discussed.\n"
          "\n"
          "ORM is designed to encourage verbalization of fact types.\n"
          "We encourage you to use such verbalizations in Task 2 as follow-up questions when the interviewees' response leaves you uncertain what fact type is intended.\n"
          "For example, in Task 2 you might have discussed a fact type corresponding to the table above with rows 'Employee No.', 'Skill', and 'Certification Date'\n"
          "But it was unclear whether or not they were keeping a history of certification dates. In this case you might ask:\n"
          "'Is it the case that you associate at most one Certification Date with each employee and skill?'\n"
          "\n"
          "The interview you conduct may prove to be rather complex and possibly long-running, but it is very important to our work, so we are giving you a big budget for question asking.\n"
          "Good luck!")
     :EADS
     {:EADS-id :data/data-basics
      :inquiry-areas
      {:comment "This is Task 1. For each area they mention, create one such object that embodies an ORM viewpoint on that domain and provides example data."
       :val [{:inquiry-area-id {:val "customer-orders"
                                :comment (str "'customer-orders' is a value in an enumeration of areas of inquiry. The enumeration values are defined as follows:\n"
                                              "\n"
                                              "'customer-orders' - about what products, quantities, due dates, expected ship dates, etc. customers are ordering.\n"
                                              "'customers' - about the customers themselves, shipping address, their standing in the company's preferred customer program, etc. This could be quite diverse!\n"
                                              "'materials' - about what materials for making product the facility has on hand, en route to the facility, or on order, expected delivery dates, etc..\n"
                                              "'bill-of-materials' - about what materials go into creating a product.\n"
                                              "'finished-goods' - about finished goods inventory.\n"
                                              "'WIP' - about work in process, its state of completion etc.\n"
                                              "'processes' - about production processes and process plans.\n"
                                              "'facilities' - about places where they make product or perform services, and what equipment is present in these places.\n"
                                              "'equipment' - about machines and tools, their capabilities, capacity, and number.\n"
                                              "'workforce' - about people, their skills, and other information about them relevant to scheduling.\n"
                                              "'holidays' - about holidays and planned periods of plant shut down.\n"
                                              "\n"
                                              "Of course, this list might not be complete for your purposes. If nothing here seems to fit, use another term and define it in a comment.")}

              :inquiry-area-objects
              {:comment "This property provides a list of objects (in the JSON sense) where each object defines an object in the ORM sense (entities) and the entity's definition."
               :val [{:object-id "product"
                      :definition {:comment "You don't have to ask the interviewees for a definition; if what is intended seems obvious just provide that."
                                   :val "a unique identifier to the product type"}}
                     {:object-id "customer"
                      :definition "the person or organization for which the product is being provided"}
                     {:object-id "promise-date"
                      :definition "The date by which the firm promised to have delivered the product to the customer."}
                     {:object-id "quantity"
                      :definition "An amount of something. (In the narrow context being defined, the quantity of product ordered."}]}

              :fact-types
              {:comment "This property provides a list of ORM fact type objects involving the inquiry-area-objects. Thus this is captures actual Task 2 ORM modeling!"
               :val [{:fact-type-id {:val "customer-has-name"
                                     :comment "You can name these however you like, as long as the name is unique among fact-type-ids."}
                      :fact-type-arity {:val 2,
                                        :comment (str "This is the arity of the relation. In this case a binary relation between a customer object and some text which is their name.\n"
                                                      "In ORM it is depicted as narrow rectangle with this number of compartments (two).\n"
                                                      "One object or data type connects to each compartment.")}
                      :fact-type-roles {:comment "This represents the role box. A "}}]}}]}}})
