(ns user
  "For REPL-based start/stop of the server.
   This file isn't used in cljs and is a problem for shadow-cljs without the
   :clj compiler directives."
  (:require
   [clojure.pprint]
   [clojure.string]
   [clojure.spec.alpha :as s]
   [clojure.tools.namespace.repl :as tools-ns :refer [disable-reload! refresh clear set-refresh-dirs]]
   [expound.alpha :as expound]
   [mount.core :as mount]
   [lambdaisland.classpath.watch-deps :as watch-deps]      ; hot loading for deps.
   [scheduling-tbd.core :refer [server]]                   ; for mount.
   [scheduling-tbd.interviewing.domain.data-analysis]                   ; for mount.
   [scheduling-tbd.interviewing.domain.process-analysis]                ; for mount.
   [scheduling-tbd.interviewing.domain.resources-analysis]              ; for mount.
   [scheduling-tbd.llm  :as llm]                           ; Because of deep synchronization problems when this is from mount.
   [scheduling-tbd.interviewing.interviewers :refer [iviewers]]         ; for mount
   [scheduling-tbd.web.handler]                            ; for mount, defines rm.server.config/config, and router stuff.
   [taoensso.telemere :as tel :refer [log!]]))

(def alias? (atom (-> (ns-aliases *ns*) keys set)))

(defn safe-alias
  [al ns-sym]
  (when (and (not (@alias? al))
             (find-ns ns-sym))
    (alias al ns-sym)))

(defn ^:diag ns-setup!
  "Use this to setup useful aliases for working in this NS."
  []
  (reset! alias? (-> (ns-aliases *ns*) keys set))
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 's      'clojure.spec.alpha)
  (safe-alias 'uni    'clojure.core.unify)
  (safe-alias 'edn    'clojure.edn)
  (safe-alias 'io     'clojure.java.io)
  (safe-alias 'str    'clojure.string)
  (safe-alias 'd      'datahike.api)
  (safe-alias 'dp     'datahike.pull-api)
  (safe-alias 'json   'jsonista.core)
  (safe-alias 'mount  'mount.core)
  (safe-alias 'p      'promesa.core)
  (safe-alias 'px     'promesa.exec)
  (safe-alias 'core   'scheduling-tbd.core)
  (safe-alias 'db     'scheduling-tbd.db)
  (safe-alias 'how    'scheduling-tbd.how-made)
  ;(safe-alias 'llm    'scheduling-tbd.llm)
  (safe-alias 'llmt   'scheduling-tbd.llm-test)
  (safe-alias 'fshop  'scheduling-tbd.interviewing.annotated-data-structures.flow-shop)
  ;(safe-alias 'pan    'scheduling-tbd.interviewing.domain.process-analysis)
  ;(safe-alias 'inv    'scheduling-tbd.interviewing.interviewers)
  ;(safe-alias 'ru     'scheduling-tbd.interviewing.response-utils)
  (safe-alias 'mzn    'scheduling-tbd.minizinc)
  (safe-alias 'mznt   'scheduling-tbd.minizinc-test)
  (safe-alias 'ou     'scheduling-tbd.op-utils)
  (safe-alias 'opt    'scheduling-tbd.operators-test)
  (safe-alias 'spec   'scheduling-tbd.specs)
  (safe-alias 'sutil  'scheduling-tbd.sutil)
  (safe-alias 'sur    'scheduling-tbd.surrogate)
  (safe-alias 'surt   'scheduling-tbd.surrogate-test)
  (safe-alias 'util   'scheduling-tbd.util)
  (safe-alias 'resp   'scheduling-tbd.web.controllers.respond)
  (safe-alias 'ws     'scheduling-tbd.web.websockets)
  (safe-alias 'tel    'taoensso.telemere)
  (safe-alias 'openai 'wkok.openai-clojure.api))


;;; If you get stuck do: (clojure.tools.namespace.repl/refresh)

;; uncomment to enable hot loading for deps
(watch-deps/start! {:aliases [:dev :test]})

(alter-var-root #'s/*explain-out* (constantly expound/printer))
(add-tap (bound-fn* clojure.pprint/pprint))
(set-refresh-dirs "src/server/scheduling_tbd" #_"test/scheduling_tbd")  ; Put here as many as you need. test messes with ns-setup!
(s/check-asserts true) ; Error on s/assert, run s/valid? rather than just returning the argument.
(tel/call-on-shutdown! tel/stop-handlers!)

(defn start
  "Start the web server"
  []
  (let [res (mount/start)
        info (str "   " (clojure.string/join ",\n    " (:started res)))]
    (log! :info (str "started:\n" info))))

(defn stop
  "Stop the web server"
  []


  (mount/stop))

(defn restart
  "Stop, reload code, and restart the server. If there is a compile error, use:

  ```
  (tools-ns/refresh)
  ```

  to recompile, and then use `start` once things are good."
  []
  (stop)
  (tools-ns/refresh :after 'user/start))
