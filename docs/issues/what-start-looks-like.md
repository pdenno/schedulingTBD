When you run (start) from the REPL, (MCP tool clojure_eval) you should see something very much like the following:

{:tools-logging
 {:present? true,
  :enabled-by-env? false,
  :sending->telemere? true,
  :telemere-receiving? true},
 :slf4j
 {:present? true,
  :telemere-provider-present? true,
  :sending->telemere? true,
  :telemere-receiving? true},
 :open-telemetry {:present? false, :use-tracer? false},
 :system/out {:sending->telemere? true, :telemere-receiving? true},
 :system/err {:sending->telemere? true, :telemere-receiving? true}}

LOG/INFO  : - Registered function for websocket: :ask-llm
LOG/INFO  : - Registered function for websocket: :set-execution-status!
LOG/INFO  : - Registered function for websocket: :start-surrogate
LOG/INFO  : - Registered function for websocket: :start-surrogate+
LOG/INFO  : - Registered function for websocket: :surrogate-follow-up
LOG/INFO  : - Registered function for websocket: :resume-conversation
LOG/INFO  : - Registered function for websocket: :set-execution-status!
LOG/INFO  : - Registered function for websocket: :ask-llm
LOG/INFO  : - Registered function for websocket: :resume-conversation
LOG/INFO  : - Registered function for websocket: :start-surrogate
LOG/INFO  : - Registered function for websocket: :start-surrogate+
LOG/INFO  : - Registered function for websocket: :surrogate-follow-up
LOG/INFO  : - Updating http handler routes.
LOG/INFO  : - Updating http handler routes.
SLF4J/INFO  : - jetty-12.0.18; built: 2025-03-13T00:46:02.267Z; git: 09e23e17430c08ed812993bebc9526dd3e67822a; jvm 21.0.7+6-Ubuntu-0ubuntu124.04
SLF4J/INFO  : - Started oeje9n.ContextHandler$CoreContextHandler@6cd21c06{ROOT,/,b=null,a=AVAILABLE,h=oeje9n.ContextHandler$CoreContextHandler$CoreToNestedHandler@6215ffec{STARTED}}
SLF4J/INFO  : - Started ServerConnector@f9c2049{HTTP/1.1, (http/1.1)}{0.0.0.0:3300}
SLF4J/INFO  : - Started oejs.Server@21b93f63{STARTING}[12.0.18,sto=0] @86564079ms
LOG/INFO  : - Started server on port 3301
LOG/INFO  : - started:
   #'scheduling-tbd.util/util-state,
    #'scheduling-tbd.web.websockets/wsock,
    #'scheduling-tbd.db/sys&proj-database-cfgs,
    #'scheduling-tbd.llm/llm-tools,
    #'scheduling-tbd.iviewr.domain.process.flow-shop/flow-shop-eads,
    #'scheduling-tbd.iviewr.domain.process.job-shop/job-shop-eads,
    #'scheduling-tbd.iviewr.domain.process.job-shop-c/job-shop-c-eads,
    #'scheduling-tbd.iviewr.domain.process.job-shop-u/job-shop-u-eads,
    #'scheduling-tbd.iviewr.domain.process.scheduling-problem-type/init-scheduling-problem-type-eads,
    #'scheduling-tbd.iviewr.domain.process.timetabling/timetabling-eads,
    #'scheduling-tbd.iviewr.domain.data.orm/orm-eads,
    #'scheduling-tbd.iviewr.domain.process.warm-up-with-challenges/init-warm-up-eads,
    #'scheduling-tbd.iviewr.interviewers/iviewrs,
    #'scheduling-tbd.surrogate/surrogates,
    #'scheduling-tbd.how-made/him-cfg,
    #'scheduling-tbd.system-agents/system-agents,
    #'scheduling-tbd.web.controllers.respond/http-responses,
    #'scheduling-tbd.web.handler/app,
    #'scheduling-tbd.core/server
