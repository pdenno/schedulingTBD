(ns user
  "This is only used for light-server.
   It contains userspace functions you can run by default in your local REPL."
  (:require
    [clojure.pprint]
    [clojure.spec.alpha :as s]
    [clojure.tools.logging :as log]
    [clojure.tools.namespace.repl :as repl]
    [criterium.core :as c]                                  ;; benchmarking
    [expound.alpha :as expound]
    [integrant.core :as ig]
    [integrant.repl :refer [clear go halt prep init reset reset-all]]
    [integrant.repl.state :as state]
    [kit.api :as kit]
    [lambdaisland.classpath.watch-deps :as watch-deps]      ;; hot loading for deps
    [mount.core :as mount]
    [scheduling-tbd.server.core :refer [start-app]]
    [scheduling-tbd.util :as util]
    [schema-db.core]))

;; uncomment to enable hot loading for deps
(watch-deps/start! {:aliases [:dev :test]})

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(add-tap (bound-fn* clojure.pprint/pprint))

(defn dev-prep!
  []
  (integrant.repl/set-prep! (fn []
                              (-> (scheduling-tbd.server.config/system-config {:profile :dev})
                                  (ig/prep)))))

(defn test-prep!
  []
  (integrant.repl/set-prep! (fn []
                              (-> (scheduling-tbd.server.config/system-config {:profile :test})
                                  (ig/prep)))))

;; Can change this to test-prep! if want to run tests as the test profile in your repl.
;; You can run tests in the dev profile, too, but there are some differences between the two profiles.
(dev-prep!)

(repl/set-refresh-dirs "src/clj")

(def refresh repl/refresh)

;;; POD added
(defn my-reset []
  (log/info "===== Exerciser reset ======")
  (mount/start) ; Start the schema-db too. See schema-db.core.
  (reset))

;;; Use start and not go, which will miss starting schema-db
(defn start []
  (util/config-log :info)
  (mount/start)
  (go))

(comment
  (go)
  (reset))
