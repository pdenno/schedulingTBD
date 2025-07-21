[
#:project{:execution-status :running,
          :conversations
          [#:conversation{:id :process,
                          :messages
                          [#:message{:content
                                     "This is where we discuss how product gets made, or in the cases of services, how the service gets delivered. It is also where we introduce MiniZinc, the <a href=\"terms/dsl\">domain specific language</a> (DSL) through which together we design a solution to your scheduling problem. You can read more about <a href=\"about/process-conversation\">how this works</a>.",
                                     :from :system,
                                     :id 1,
                                     :tags [:conversation-intro],
                                     :time
                                     #inst "2025-06-19T12:39:31.744-00:00"}]
                          :status :in-progress}
           #:conversation{:id :data,
                          :messages
                          [#:message{:content
                                     "This is where we ask you to talk about the data that drives your decisions (customer orders, due dates, worker schedules,... whatever). Here you can either upload actual data as spreadsheets, or we can talk about the kinds of information you use in general terms and we can invent some similar data to run demonstrations. Whenever someone suggests that you upload information to them, you should be cautious. Read more about the intent of this conversation and the risks of uploading data <a href=\"about/uploading-data\">here</a>.",
                                     :from :system,
                                     :id 1,
                                     :tags [:conversation-intro],
                                     :time
                                     #inst "2025-06-19T12:39:31.760-00:00"}],
                          :status :not-started}
           #:conversation{:id :resources,
                          :messages
                          [#:message{:content
                                     "This is typically the third conversation we'll have, after discussing process and data. (By the way, you can always go back to a conversation and add to it.) You might have already mentioned the resources (people, machines) by which you make product or deliver services. Here we try to integrate this into the MiniZinc solution. Until we do that, we won't be able to generate realistic schedules.",
                                     :from :system,
                                     :id 1,
                                     :tags [:conversation-intro],
                                     :time
                                     #inst "2025-06-19T12:39:31.782-00:00"}],
                          :status :not-started}
           #:conversation{:id :optimality,
                          :messages
                          [#:message{:content
                                     "This is where we discuss what you intend by 'good' and 'ideal' schedules. With these we formulate an objective and model it in MiniZinc. The MiniZinc solution can change substantially owing to this discussion, but owing to all the work we did to define requirements, we think it will be successful.",
                                     :from :system,
                                     :id 1,
                                     :tags [:conversation-intro],
                                     :time
                                     #inst "2025-06-19T12:39:31.801-00:00"}],
                          :status :not-started}],
          :active-conversation :process,
          :agents
          [#:agent{:base-type :data-interview-agent,
                   :agent-type :shared-assistant,
                   :tools "[{:type \"file_search\"}]",
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :vector-store-paths
                   ["resources/agents/iviewrs/papers/object-role-modeling--an-overview.pdf"],
                   :instruction-path
                   "/tmp/stbd-agents/data-interviewer.txt",
                   :timestamp #inst "2025-06-03T20:46:51.756-00:00",
                   :agent-id :data-interview-agent-openai}
           #:agent{:base-type :data-interview-agent,
                   :agent-type :shared-assistant,
                   :tools "[{:type \"file_search\"}]",
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :vector-store-paths
                   ["resources/agents/iviewrs/papers/object-role-modeling--an-overview.pdf"],
                   :instruction-path
                   "/tmp/stbd-agents/data-interviewer.txt",
                   :assistant-id "asst_aH2xqHWSW2mTGI8oLKpWKuj1",
                   :timestamp #inst "2025-06-17T16:30:57.694-00:00",
                   :agent-id :data-interview-agent}
           #:agent{:base-type :orchestrator-agent,
                   :agent-type :shared-assistant,
                   :tools "[{:type \"file_search\"}]",
                   :thread-id "thread_XJlO5sUaZnps7z4PLHOfPC0P",
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :vector-store-paths
                   ["resources/agents/iviewrs/EADS/data/orm.json"
                    "resources/agents/iviewrs/EADS/process/flow-shop.json"
                    "resources/agents/iviewrs/EADS/process/job-shop--classifiable.json"
                    "resources/agents/iviewrs/EADS/process/job-shop--unique.json"
                    "resources/agents/iviewrs/EADS/process/job-shop.json"
                    "resources/agents/iviewrs/EADS/process/scheduling-problem-type.json"
                    "resources/agents/iviewrs/EADS/process/timetabling.json"
                    "resources/agents/iviewrs/EADS/process/warm-up-with-challenges.json"],
                   :instruction-path
                   "resources/agents/orchestrator.txt",
                   :assistant-id "asst_LjoLe6MTETDpP5fWb3yRoXrW",
                   :timestamp #inst "2025-06-19T12:43:00.075-00:00",
                   :agent-id :orchestrator-agent}
           #:agent{:base-type :optimality-interview-agent,
                   :agent-type :shared-assistant,
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :instruction-path
                   "/tmp/stbd-agents/optimality-interviewer.txt",
                   :assistant-id "asst_akK3OGJBOiXtsFcH4CAWdsIe",
                   :timestamp #inst "2025-06-04T12:45:21.614-00:00",
                   :agent-id :optimality-interview-agent}
           #:agent{:base-type :process-interview-agent,
                   :agent-type :shared-assistant,
                   :thread-id "thread_cEdZ2QcCFeIfV4APd9HKdXTk",
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :instruction-path
                   "/tmp/stbd-agents/process-interviewer.txt",
                   :assistant-id "asst_wcjVUlESZYawvPRfh84t35a0",
                   :timestamp #inst "2025-06-19T12:40:53.455-00:00",
                   :agent-id :process-interview-agent}
           #:agent{:base-type :resources-interview-agent,
                   :agent-type :shared-assistant,
                   :model-class :gpt,
                   :llm-provider :openai,
                   :pid :sur-craft-beer,
                   :instruction-path
                   "/tmp/stbd-agents/resources-interviewer.txt",
                   :assistant-id "asst_Z8OIRCCEGCPNfQOnuSGsb85V",
                   :timestamp #inst "2025-06-04T12:45:22.720-00:00",
                   :agent-id :resources-interview-agent}
           #:agent{:base-type :sur-craft-beer,
                   :agent-type :project,
                   :thread-id "thread_E1kT66xC5yMd59gdM3pgAkFd",
                   :model-class :gpt,
                   :instruction-string
                   "You manage a company that makes craft beer.\nYou are an expert in production and manage your company's supply chains.\nYou help me by answering an interviewer's questions that will allow us to collaborate in building a scheduling system for your company.\nCurrently, you do not use any software in scheduling your work, except maybe spreadsheets and office software calendars.\nYour answers typically are short, just a few sentences each.\n\nTypically you answer in sentences. However, the interviewers may ask you provide a table or add information to a table that the interviewer provides.\nIn these cases, respond with an HTML table wrapped in #+begin_src HTML ... #+end_src\nAll the tables you generate / complete should have the header as the first table row.\nFor example if you were asked to generate / complete a table about process durations for process steps you might respond with:\n\"Here are my estimates of the step durations. Note that I added --Yet another step-- which I think should be included in this context:\"\n\n#+begin_src HTML\n<table>\n  <tr><th>Process Step</th>                <th>Duration</th></tr>\n  <tr><td>--Some step--</td>               <td>2 hours</td></tr>\n  <tr><td>--Some other step--</td>         <td>3 days</td></tr>\n  <tr><td>--Yet anoter step--</td>         <td>1 days</td></tr>\n</table>\n#+end_src",
                   :llm-provider :openai,
                   :surrogate? true,
                   :pid :sur-craft-beer,
                   :assistant-id "asst_hfwDF9q1Y0tNufXZosVYCpVv",
                   :expertise "craft beer",
                   :timestamp #inst "2025-06-19T12:40:54.692-00:00",
                   :agent-id :sur-craft-beer-openai}],
          :claims
          [#:claim{:string "(project-id :sur-craft-beer)"}
           #:claim{:string
                   "(project-name :sur-craft-beer \"SUR Craft Beer\")"}
           #:claim{:conversation-id :process,
                   :string "(surrogate :sur-craft-beer)"}],
          :name "SUR Craft Beer",
          :id :sur-craft-beer}
]
