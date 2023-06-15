(ns stbd-app.components.save-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [clojure.pprint             :refer [cl-format]]
   [helix.core :refer [defnc $]]
   [helix.hooks :as hooks]
   [stbd-app.util :refer [component-refs]]
   [stbd-app.components.editor :refer [set-editor-text get-editor-text]]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/icons-material/Save$default" :as Save]
   ["@mui/material/Typography$default" :as Typography]
   ["@mui/material/Modal$default" :as Modal]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def style (clj->js
            {:position "absolute", ; as 'absolute'
             :top "50%",
             :left "50%",
             :transform "translate(-50%, -50%)",
             :width 650,
             :bgcolor "background.paper",
             :border "2px solid #000",
             :boxShadow 24,
             :p 2}))

(def white-style (clj->js {:color "background.paper"}))
(def svr-prefix "http://localhost:3000/app/")
(def diag (atom nil))

;;; https://mui.com/material-ui/react-modal/
(defnc SaveModal [{:keys [code-fn data-fn]}]
  (let [[open, set-open] (hooks/use-state false)
        [url set-url]    (hooks/use-state nil)
        [text set-text]  (hooks/use-state "")
        modal            (hooks/use-ref nil)]
    (letfn [(save-success [{:keys [save-id]}]
              (when (j/get modal :current)
                (js/console.log "User code saved at:"
                (set-url (str svr-prefix "example/" save-id))
                (set-text "To recover the work shown visit:")
                (set-open true))))
            (save-failure [status status-text]
              (when (j/get modal :current)
                (log/info "Saving example failed: status = " status " status text = " status-text)
                (set-text "Communication with the server failed.")
                (set-open true)))
            (handle-save []
              (POST "/api/example"
                    {:params {:code (code-fn) :data (data-fn)}
                     :timeout 3000
                     :handler       save-success
                     :error-handler save-failure}))
            (handle-close [] (set-open false))]
      ($ "div" {:ref modal}
         ($ IconButton {:onClick handle-save} ($ Save {:sx white-style}))
         ($ Modal {:open open
                   :onClose handle-close
                   :aria-labelledby "save-modal-title"
                   :aria-describedby "save-modal-description"}
            ($ Box {:sx style}
               ($ Typography {:id "save-modal-title" :variant "h6" :component "h6"} text)
               ($ Typography {:id "save-modal-description" :sx {:mt 20}} url)))))))

(def result-atm (atom nil))

;;; ($read [["schema/name" "urn:oagis-10.8:Nouns:Invoice"],  ["schema-object"]])
(def test-obj
   {:ident-type "schema/name"
    :ident-val "urn:oagis-10.8.4:Nouns:Invoice"
    :request-objs "schema-object"})

(defn tryme [] ; Modern promesa with p/do, The winner!
  (let [p (p/deferred)]
    (p/do
      (log/info "Do this first")
      (p/do
        (log/info "Do this next")
        (GET "http://localhost:3000/api/graph-query" ; ToDo: Need localhost:3000 (exerciser) here?
             {:params test-obj
              :handler (fn [resp]
                         (log/info (str "CLJS-AJAX returns ok."))
                         (reset! result-atm resp)
                         (p/resolve! p resp)) ; This isn't 'returned'!
              :error-handler (fn [{:keys [status status-text]}]
                               (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                               (reset! result-atm :failure!)
                               (p/reject! p (ex-info "CLJS-AJAX error on /api/graph-query"
                                                     {:status status :status-text status-text})))
              :timeout 3000})
        p ; It waits? here
        (log/info "'***$read(graph-query)' returns " (-> @result-atm str (subs 0 100) (str "...")))
        (log/info "Do this penultimately."))
      (log/info "Do this last"))))


;;; Flawed? p/do needs a promise?
#_(defn tryme [] ; Modern promesa with p/do doesn't await for ***$read and doesn't return the @result-atm (but sets it).
  (let [p (p/deferred)]
    (p/do (GET "/api/graph-query" ; ToDo: Need localhost:3000 (exerciser) here?
               {:params test-obj
                :handler (fn [resp]
                           (log/info (str "CLJS-AJAX returns:" resp))
                           (reset! result-atm resp)
                           (p/resolve! p resp)) ; Useless?
                :error-handler (fn [{:keys [status status-text]}]
                                 (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                                 (reset! result-atm :failure!)
                                 (p/reject! p (ex-info (str "CLJS-AJAX error: status = " status " status-text= " status-text) {})))
                :timeout 3000})
          (log/info "'***$read(graph-query)' returns " @result-atm)
           @result-atm)))

#_(defn tryme [] ; Modern promesa doesn't have a cljs await.
  (let [p (p/deferred)]
    (async
     (await (GET "/api/graph-query" ; ToDo: Need localhost:3000 (exerciser) here?
                  {:params test-obj
                   :handler (fn [resp]
                              (log/info (str "CLJS-AJAX returns:" resp))
                              (reset! result-atm resp)
                              (p/resolve! p resp))
                   :error-handler (fn [{:keys [status status-text]}]
                                    (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                                    (reset! result-atm :failure!)
                                    (p/reject! p (ex-info (str "CLJS-AJAX error: status = " status " status-text= " status-text) {})))
                   :timeout 3000}))
     (log/info "'***$read(graph-query)' returns " @result-atm)
     @result-atm)))

#_(defn tryme [] ; shadow: Sets atom but doesn't execute the await part at all.
    (js-await [res (GET "/api/graph-query"
                        {:params test-obj
                         :handler (fn [resp]
                                    ;(log/info (str "CLJS-AJAX returns: " resp))
                                    (reset! result-atm resp))
                         :error-handler (fn [{:keys [status status-text]}]
                                          (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                                          (reset! result-atm :failure!))
                         :timeout 3000})]
              (log/info "'***$read(graph-query)' returns " @result-atm)
              @result-atm))


#_(defn tryme [] ; Ancient promesa doesn't have reject etc. This doesn't set result-atm and doesn't await in the sense that the ****$read log happens immediately.
  (let [result-atm (atom nil)] ;<========================
    (async
     (await (p/promise (GET "/api/graph-query"
                          {:params test-obj
                           :handler (fn [resp]
                                      (log/info (str "CLJS-AJAX returns:" resp))
                                      (reset! result-atm resp))
                           :error-handler (fn [{:keys [status status-text]}]
                                            (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                                            (reset! result-atm :failure!))
                           :timeout 3000})))
     (log/info "'***$read(graph-query)' returns " @result-atm)
     @result-atm)))

#_(defn tryme [] ;; go-loop sets atom. Doesn't await.
  (let [ch (chan 3)]
    (try
      (GET "/api/graph-query" ; ToDo: Need localhost:3000 (exerciser) here?
           {:params test-obj
            :handler (fn [resp]
                       (log/info (str "CLJS-AJAX returns:" resp))
                       (go (>! ch resp)))
            :error-handler (fn [{:keys [status status-text]}]
                             (log/info (str "CLJS-AJAX error: status = " status " status-text= " status-text))
                             (close! ch))
            :timeout 3000})
      (let [res (go-loop []
                  (<! (timeout 3000))
                  (if-let [res (<! ch)]
                    (do (reset! result-atm res)
                        (log/info "'***$read(graph-query)' returns " @result-atm)
                        (close! ch))
                    (recur)))]
        (reset! diag res)
        res)
      (finally
        (log/info "Clean-up")))))
