(ns scheduling-tbd.interviewing.domain.data.data-basics
  "Planning operators for the data interview"
  (:require
   [clojure.spec.alpha    :as s]
   [datahike.api          :as d]
   [mount.core :as mount :refer [defstate]]
   [scheduling-tbd.db] ;for mount
   [scheduling-tbd.sutil  :as sutil :refer [connect-atm clj2json-pretty]]))


(def data
    {:message-type :EADS-INSTRUCTIONS
     :interviewer-agent :data
     :interview-objective
     (str "The data interview is about discovering the information that interviewees use to do their work (scheduling production or providing a service to customers).\n"
          "Many small manufacturers use spreadsheets to run their company.\n"
          "It probably would be expeditious to have the interviewees upload their spreadsheets, but we are not going to take that route.\n"
          "Instead, we are going to try to create example data of the sort that they described to you.\n"
          "We will use this data to run prototypes of the MiniZinc-based scheduling system that the humans and AI create together.\n"
          "\n"
          "There are three principal tasks to be achieved in this interview:\n"
          "   Task 1: determining the kinds and scope of data involved in decision making in the interviewees' work,\n"
          "   Task 2: determining the relationships among the data, and,\n"
          "   Task 3: creating examples of the data that the interviewees would deem realistic.\n"
          "\n"
          "Regarding Task 1, the kind and scope of data, we are concerned primarily with the categorization of quantitative and/or structured data.\n"
          "We provide below a categorization, but, as always, you can use annotations as described earlier to impart nuance to what you learned from the interviewees.\n"
          "Likewise you can used the 'invented' property to add to our categorization when needed.\n"
          "\n"
          "Regarding Task 2, the relationships among the data, we are particularly interested in capturing its domain semantics in the viewpoint of Object Role Modeling (ORM).\n"
          "This includes capturing functional relationships among elements, for example, determining that when talking about an employee's skills, we can unambiguosly refer to the employee using their 'employee number'.\n"
          "When talking about when an employee was last certified for a skill, we would use 'employee number' and 'skill type' (say, a value in an enumeration) together as a key (a 'concatenated key') to the certification date.\n"
          "This concatenated key is the functional mapping to certification date.\n"
          "ORM is designed to encourage verbalization of fact types, and to use these verbalizations (reading of an ORM fact type) to confirm your understanding of what the interviewees are attempting to convey.\n"
          "We encourage you to use verbalizations.\n"
          "The first chapter of the book 'Object Role Modeling Fundamentals' (provided to you) and especially Table 1.2 in the book, might help you with this.\n"
          "\n"
          "Regarding Task 3, creating realistic examples of the data, we'd like to remind you that you can present tabular data to the interviewees.\n"
          "The method to do this is to include a HTML table in your questions bounded by #+begin_src HTML ... #+end_src.\n"
          "An example of this is below:\n"
          "\n"
          "#+begin_src HTML\n"
          "<table>\n"
          "   <tr><th>Employee No.</th>        <th>Skill</th>               <th>Certification Date</th></tr>\n"
          "   <tr><td>N123</td>                <td>Milling Center</td>      <td>2024-10-05             </tr>\n"
          "   <tr><td>N098</td>                <td>Milling Center</td>      <td>2022-11-13             </tr>\n"
          "   <tr><td>N891</td>                <td>EDM machines</td>        <td>2023-03-28             </tr>\n"
          "</table>\n"
          "#+end_src\n"
          "\n"
          "We are able to read the tables you provide into a nice UI component that allows the users to edit the content of cells and add and remove rows.\n"
          "\n"
          "We encourage you to start the interview with an open-ended question about what kinds of data they use, for example:\n"
          "\n"
          "   'To get started, could you list the kinds of data that you use to schedule production?\n"
          "    For example, do you have speadsheets containing customer orders, raw material delivery, process plans, task durations, worker skills, etc.\n"
          "\n"
          "You can then discuss each area they mention (we call them 'inquiry-areas below), in whatever order you deem appropriate, discussing the details (1) and (2) and generating examples (3)."
          "When you have completed the detailed discussion of everything they have discussed, you should ask them whether any other area of data has come to mind that we should discuss\n"
          "If they mention more, continue to apply the three tasks until all data areas are discussed.\n"
          "\n"
          "The interview you conduct may prove to be rather complex and possibly long-running, but it is very important to our work, so we are giving you a big budget for question asking.\n"
          "Good luck!")
     :EADS
     {:EADS-id :data/data-basics
      :inquiry-areas {:comment "For each area they mention, create one such object that embodies an ORM viewpoint on that domain and provides example data."
                      :val [{:inquiry-area-id {:val "customer-orders"
                                               :comment (str "'customer-orders' is a value in an enumeration. The enumeration values are defined as follows:\n"
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
                             :inquiry-area-objects {:comment "This property provides a list of objects (in the JSON sense) where each object defines an object in the ORM sense (entities) and the entity's definition."
                                                    :val [{:object-id "product"
                                                           :definition {:comment "You don't have to ask the interviewees for a definition; if what is intended seems obvious just provide that."
                                                                        :val "a unique identifier to the product type"}}
                                                          {:object-id "customer"
                                                           :definition "the person or organization for which the product is being provided"}
                                                          {:object-id "promise-date"
                                                           :definition "The date by which the firm and customer agreed the product shall be delivered."}
                                                          {:object-id "quantity"
                                                           :definiiton "An amount of something. (In the narrow context being defined, the quantity of product ordered."}]}
                             :fact-types {:comment "This property provides a list of ORM fact types involving the inquiry-area-objects. Thus this is actual ORM modeling! Task 2!"
                                          :val []}}]}}})
