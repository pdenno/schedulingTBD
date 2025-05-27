(ns scheduling-tbd.web.handler
  "HTTP router and handlers. Currently you have to user/restart (or similar) for changes to be seen."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [mount.core :as mount :refer [defstate]]
   [muuntaja.core :as m]
   [ring.middleware.defaults :as defaults]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.session.cookie :as cookie]
   [reitit.ring :as ring]
   [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
   [ring.util.http-response :refer [content-type ok]]
   [reitit.http :as http]
   [reitit.coercion.spec]
   [reitit.swagger :as swagger]
   [reitit.swagger-ui :as swagger-ui]
   [reitit.http.coercion :as coercion]
   [reitit.dev.pretty :as pretty]
   [reitit.interceptor.sieppari :as sieppari]
   [reitit.http.interceptors.parameters :as parameters] ; This one stays!
   [reitit.http.interceptors.muuntaja :as muuntaja]
   [reitit.http.interceptors.exception :as exception]
   [reitit.http.interceptors.multipart :as multipart]
   [reitit.http.interceptors.dev :as dev] ; for testing
   [reitit.http.spec :as spec]
   [scheduling-tbd.web.controllers.respond :as resp :refer [http-responses]] ; For mount
   [scheduling-tbd.web.websockets   :as wsock]
   [selmer.parser :as parser] ; kit influence
   [spec-tools.core  :as st]
   ;;[spec-tools.swagger.core :as swag2] ; Experimental
   [spec-tools.spell :as spell]
   [taoensso.telemere :refer [log!]]))

;;; Reitit: (pronounced "rate it") a routing library.
;;;   - https://github.com/metosin/reitit, (docs)
;;;   - https://www.youtube.com/watch?v=cSntRGAjPiM  (50-minute video)
;;;   - https://www.slideshare.net/metosin/reitit-clojurenorth-2019-141438093 (slides for the above)

;;; Notes:
;;;   * This stuff is a bit out of touch with best practice, which probably uses Malli: https://github.com/metosin/malli/tree/master
;;;   * You can specify keywords and keywords will be conveyed to the web app, but the swagger API will show them as strings.
;;;   * If the swagger UI (the page you get when you GET index.html) displays "Failed to load API definition", try :no-doc true
;;;     on the most recent definition (or many of them until things start working).
;;;   * You don't have to restart the server to see changes in the swagger page; just recompile it.
;;    * However, to see updates to referenced functions (e.g. the stuff in respond.clj) you need to restart the server: (user/restart).
;;;   * A common reason for the Swagger page failing is that you didn't completely specify the spec defs referenced.
;;;     By this I mean that, for example, you specified :req-un [some-key], but you don't have a spec for some-key.
;;;   * The devl/ajax-tests are Work In Process. Some have uninvestigated bugs.
;;;   * You can choose not to define specs for the keys. (Really??? I'm not so sure!)
;;;   * The most useful debugging techniques seem to be (in order):
;;;       1) Get the handler definition good enough that it displays in the swagger UI.
;;;       2) Execute with swagger and determine whether problem is with Clojure Spec validation.
;;;       3) If it is get diag atoms for the offending request or response and run s/valid? and s/explain by hand.
;;;       4) If none of that is the problem, study what the wrappers are doing by uncommenting ev/print-context-diffs and running the code.
;;;   * I once had what appeared to be non-printing text in a s/def expression mess things up. Really.

;;; =========== Pages (just homepage, thus far.)  =======
(def selmer-opts {:custom-resource-path (io/resource "html")})

(defn render
  [_request template & [params]]
  (-> (parser/render-file template
                          (assoc params :page template :csrf-token *anti-forgery-token*)
                          selmer-opts)
      (ok)
      (content-type "text/html; charset=utf-8")))

(defn home [{:keys [flash] :as request}]
  (render request #_"tryme.html" "home.html" {:errors (:errors flash)}))
;;;====================================================

(s/def ::client-id (st/spec {:spec string? ; ToDo: Switch to bare uuid when switch to transit.
                             :name "client-id"
                             :description "A UUID string identifying the client."
                             :json-schema/default "2f30f002-37b7-4dd1-bc01-5484273012f0"}))

;;; --------- (require '[develop.dutil :as devl]) ; Of course, you'll have to (user/restart) when you update things here.
;;; --------- Try it with :reitit.interceptor/transform dev/print-context-diffs. See below.
;;; --------- (devl/ajax-test "/api/user-says" {:user-text "LLM: What's the capital of Iowa?"} {:method ajax.core/POST})
(s/def :message/content (s/coll-of map?))
(s/def :message/id integer?)
(s/def :message/from keyword?)
(s/def :message/time inst?)

;;; -------- (devl/ajax-test "/api/list-projects" [])
(s/def ::others (s/coll-of map?))
(s/def ::current-project map?)
(s/def ::cid keyword?)
(s/def ::list-projects-response (s/keys :req-un [::others ::current-project ::cid]))

;;; -------- (devl/ajax-test "/api/get-conversation" {:project-id "craft-beer-brewery-scheduling"})
(s/def ::cid (st/spec {:spec keyword?
                       :name "cid"
                       :description "one of \"process\" \"data\" or \"resource\"."
                       :json-schema/default "process"}))

(s/def ::get-conversation-request (st/spec {:spec (s/keys :req-un [::client-id ::project-id] :opt-un [::cid])
                                            :name "project-id"
                                            :description "A string uniquely identifying the project to the system DB."
                                            :json-schema/default {:project-id "sur-craft-beer"
                                                                  :cid "process"
                                                                  :client-id "2f30f002-37b7-4dd1-bc01-5484273012f0"}}))

(s/def ::conv (s/coll-of map?))
(s/def ::mzn-output string?)
(s/def ::get-conversation-response (s/keys :req-un [::project-id ::project-name ::conv ::cid]))
(s/def ::project-id (st/spec {:spec #(or (string? %) (keyword? %))
                              :name "project-id"
                              :description "A kebab-case string (will be keywordized) unique to the system DB identifying a project."
                              :json-schema/default "sur-craft-beer"}))

#_(s/def ::project-name (st/spec {:spec string?
                              :name "project-name"
                              :description "The name of a project."
                              :json-schema/default "sur-craft-beer"}))

(s/def ::run-minizinc-request (st/spec {:spec (s/keys :req-un [::client-id ::project-id])
                                        :name "project-id"
                                        :description "A string uniquely identifying the project to the system DB."
                                        :json-schema/default {:project-id "sur-craft-beer"
                                                              :client-id "2f30f002-37b7-4dd1-bc01-5484273012f0"
                                                              :body {:code (-> "data/examples/small2-pred.mzn" slurp)}}}))

(s/def ::run-minizinc-response (st/spec {:spec (s/keys :req-un [[::mzn-output]])
                                        :name "run-minizinc-response"
                                        :description "Returned string from running minizinc"
                                        :json-schema/default {:mzn-output "Some output"}}))


;;; -------- (devl/ajax-test "/api/ws" {:client-id "my-fake-uuid"}) ; ToDo: ajax-test not working here. Not investigated
(s/def ::ws-connection-request (s/keys :req-un [::client-id]))

(s/def :project/id keyword?)
(s/def :project/name string?)

(s/def ::size int?)
(s/def ::name string?)
(s/def ::upload-file-response (st/spec {:spec (s/keys :req-un [[::name ::size]])
                                        :name "upload-file-reponse"
                                        :description "Upload a file"
                                        :json-schema/default {:name "foo.png" :size 100}}))
(def routes
  [["/app" {:get {:no-doc true
                  :summary "Load the web app for someone."
                  :handler home}}]

   ["/swagger.json"
     {:get {;:no-doc true
            :swagger {:info {:title "SchedulingTBD API"
                             :description "API with reitit-http"}}
            :handler (swagger/create-swagger-handler)}}]

   ["/ws"
    {:get {:no-doc true
           :summary "Web socket connection request"
           :parameters {:query ::ws-connection-request}
           :handler wsock/establish-websocket-handler}}]

   ["/files"
    {:swagger {:tags ["files"]}}

    ;; ToDo: Fix Swagger, request needs project-id
    ["/upload" ; In Swagger you can test this with real files.
     {:post {;:no-doc true
             :summary "upload a file"
             :parameters {:multipart {:file multipart/temp-file-part}}
             :responses {200 {:body ::upload-file-response}}
             :handler resp/upload-file}}]]
   ["/api"
    {:swagger {;:no-doc true
               :tags ["SchedulingTBD functions"]}}

    ["/run-minizinc" ; In Swagger you can test this with real files.
     {:post {;:no-doc true
             :summary "Run the posted minizinc"
             :parameters {:query ::run-minizinc-request}
             :responses {200 {:body ::run-minizinc-response}}
             :handler resp/run-minizinc}}]

    ["/get-conversation"
     {:get {;:no-doc true
            :summary "Get the project's conversation from its project DB."
            :parameters {:query ::get-conversation-request}
            :responses {200 {:body ::get-conversation-response}}
            :handler resp/get-conversation}}]

    ["/list-projects"
     {:get {;:no-doc true
            :summary "Get a vector of projects maps and the current project."
            :responses {200 {:body ::list-projects-response}}
            :handler resp/list-projects}}]

    ["/health"
     {:get {;:no-doc true
            :summary "Check server health"
            :responses {200 {:body {:time string? :up-since string?}}}
            :handler resp/healthcheck}}]]])

(def options
  {;:reitit.interceptor/transform dev/print-context-diffs ;<======= pretty context diffs of the request (but slows things down quite a bit)!
   :validate spec/validate ;; enable spec validation for route data
   :reitit.spec/wrap spell/closed ;; strict top-level validation  (error reported if you don't have the last two interceptors)
   :exception pretty/exception
   :data {:coercion reitit.coercion.spec/coercion
          :muuntaja m/instance
          :interceptors [;; swagger feature
                         swagger/swagger-feature
                         ;; query-params & form-params
                         (parameters/parameters-interceptor)
                         ;; content-negotiation
                         (muuntaja/format-negotiate-interceptor)
                         ;; encodeing response body                ; This one will take :body object (e.g. a map) and return ad java.io.ByteArrayInputStream
                         (muuntaja/format-response-interceptor)    ; Nothing past here reports anything through print-context-diffs.
                         ;; exception handling
                         (exception/exception-interceptor)
                         ;; decoding request body
                         (muuntaja/format-request-interceptor)
                         ;; coercing response bodies
                         (coercion/coerce-response-interceptor)
                         ;; coercing request parameters
                         (coercion/coerce-request-interceptor)
                         ;; multipart
                         (multipart/multipart-interceptor)]}})

(def default-routes
  "The swagger examples meaningful and therefore good tests.
   However, keep in mind that they don't test calls from CLJS!"
  (ring/routes
   (swagger-ui/create-swagger-ui-handler ;; Creates a ring handler which can be used to serve swagger-ui.
    {:path "/"
     :config {:validatorUrl nil
              :operationsSorter "alpha"}})
   (ring/create-resource-handler {:path "/"})
   (ring/create-default-handler)))

(defn handler-init []
  (log! :info "Updating http handler routes.")
  (let [site-config (-> "system.edn" io/resource slurp edn/read-string :dev :handler/ring)
        s ^String (:cookie-secret site-config)
        cookie-store (cookie/cookie-store {:key (.getBytes s)})
        app (-> (http/ring-handler
                 (http/router routes options)
                 default-routes
                 {:executor sieppari/executor})

                (defaults/wrap-defaults
                 (assoc-in site-config [:session :store] cookie-store))

                ;; For Kaocha testing through port 1818, at least." 3300 for devl/ajax-test.
                (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                           :access-control-allow-methods [:get :put :post :delete]))]
    app))

(defstate app
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (handler-init))
