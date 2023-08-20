(ns scheduling-tbd.web.handler
  (:require
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
   [scheduling-tbd.web.controllers.respond :as resp]
   [selmer.parser :as parser] ; kit influence
   [spec-tools.core  :as st]
   [spec-tools.spell :as spell]
   [taoensso.timbre  :as log]))

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
  (render request "home.html" {:errors (:errors flash)}))
;;;====================================================

(s/def ::user-text string?)
(s/def ::user-says-request (st/spec {:spec (s/keys :req-un [::user-text])
                                     :name "text"
                                     :description "The text of the user's message."
                                     :json-schema/default ""}))

(s/def ::user-says-response (s/keys :req [:message/id :message/text :message/from]))

;;; You can choose not to define specs for the keys.
(s/def ::initial-projects-response (s/keys
                                    :opt-un [::projects ::current-project]
                                    :req-un [::initial-prompt]))

(def routes
  [["/app" {:get {:summary "Ignore this swagger entry. I will get rid of it someday."
                              :handler home}}]
   ["/swagger.json"
     {:get {:no-doc true
            :swagger {:info {:title "RADmapper API"
                             :description "API with reitit-http"}}
            :handler (swagger/create-swagger-handler)}}]

   ["/api"
    {:swagger {:tags ["RADmapper functions"]}}

   ["/user-says"
     {:post {:summary "Respond to the user's most recent message."
             :parameters {:body ::user-says-request}
             :responses {200 {:body ::user-says-response}}
             :handler resp/respond}}]

    ["/initial-projects"
     {:get {:summary "Respond to the user's most recent message."
            :responses {200 {:body ::initial-projects-response}}
            :handler resp/initial-projects}}]

    ["/health"
     {:get {:summary "Check server health"
            :responses {200 {:body {:time string? :up-since string?}}}
            :handler resp/healthcheck}}]]])

(def options
  {;:reitit.interceptor/transform dev/print-context-diffs ;; pretty context diffs
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
                         (muuntaja/format-response-interceptor)    ; Nothing past here reports anything trough print-context-diffs.
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
   (swagger-ui/create-swagger-ui-handler
    {:path "/"
     :config {:validatorUrl nil
              :operationsSorter "alpha"}})
   (ring/create-resource-handler {:path "/"})
   (ring/create-default-handler)))

(defn handler-init []
  (let [site-config (-> "system.edn" io/resource slurp read-string :dev :handler/ring)
        s ^String (:cookie-secret site-config)
        cookie-store (cookie/cookie-store {:key (.getBytes s)})
        app (-> (http/ring-handler
                 (http/router routes options)
                 default-routes
                 {:executor sieppari/executor})

                (defaults/wrap-defaults
                 (assoc-in site-config [:session :store] cookie-store))

                ;; For Kaocha testing through port 1818, at least."
                (wrap-cors :access-control-allow-origin [#"http://localhost:1818"]
                           :access-control-allow-methods [:get :put :post :delete]))]
    app))

(defstate app
  "Reitit Ring handler (a self-sufficient 'app' sans listening on port)."
  :start (handler-init))
