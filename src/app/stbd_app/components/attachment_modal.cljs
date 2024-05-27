(ns stbd-app.components.attachment-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Buttons$default" :refer [AttachmentButton]]
   [helix.core :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   ["react-dropzone/dist/index$useDropzone" :as useDropzone]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Card$default" :as Card]
   ["@mui/material/Divider$default" :as Divider]
   ["@mui/material/Input$default" :as Input]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/Typography$default" :as Typography]
   ["@mui/material/Modal$default" :as Modal]
   ["@mui/material/Dialog$default" :as Dialog]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

(defn size-string
  [size]
  (cond (< size 1024)         (str size " b")
        (< 1024 size 1048576) (str (int (/ size 1024)) " k")
        :else                 (str (-> size (/ 1048576) int) "."
                                   (-> (/ (rem size 1048576) 1048577) (* 100) int) " M")))
;;; https://mui.com/material-ui/react-modal/
;;; https://react-dropzone.js.org/#section-basic-example
(defnc AttachmentModal []
  (let [[open, set-open] (hooks/use-state false)
        [state-text set-state-text]  (hooks/use-state "HEY!")
        modal            (hooks/use-ref nil)
        dz-hook         (useDropzone)
        root-props      ((j/get dz-hook :getRootProps) #js {:className "dropzone"})
        input-props     ((j/get dz-hook :getInputProps))
        accepted-files  (j/get dz-hook :acceptedFiles)
        files           (atom [])]
    (letfn [(save-success [{:keys [save-id]}]
              (when (j/get modal :current)
                (set-open false)))
            (save-failure [status status-text]
              (when (j/get modal :current)
                (log/info "Saving example failed: status = " status " status text = " status-text)
                (set-state-text "Communication with the server failed.")
                (set-open true)))
            (handle-attach []
              (when (j/get modal :current)
                (set-open true)
                (set-state-text "Drag a file here to upload it.")))
            (upload-files []
              (doseq [f accepted-files]
                (log/info "upload file:" (j/get f :name))
                (let [fr (js/FileReader.)]
                  (.addEventListener fr "loadend"
                                     (fn [_event]
                                       (let [url (j/get fr :result)]
                                         (log/info "url = " url)
                                         (-> (js/fetch url)      ; returns a promise
                                             (p/then #(.text %)) ; returns a promise
                                             (p/then #(do (log/info "Uploading: content =" %) %))
                                             (p/then #(POST "/files/upload"
                                                            {:params {:filename (j/get f :name) :size (j/get f :size)}
                                                             :body %
                                                             :timeout 3000
                                                             :handler       save-success
                                                             :error-handler save-failure}))))))
                  ;; This causes the "loadend" event.
                  (.readAsDataURL fr f))))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
        ($ AttachmentButton {:onClick handle-attach})
        ($ Dialog  {:open open :onClose handle-close}
           ($ Stack {:direction "row"
                     :divider ($ Divider {:variant "activeVert" :color "black"})} ; ToDo: only "activeVert" works.
              ($ Box {:& root-props :sx #js {:padding  "40px 10px 10px 10px;"
                                             :bgcolor "#f0e699"}}
                 ($ Card {:elevation 5}
                    ($ Input {:& input-props})
                    ($ Typography {:sx #js {:padding  "40px 40px 30px 50px;"} ; top right bottom left
                                   :id "save-modal-title"
                                   :variant "h6"
                                   :component "h6"}
                       "Drag and drop files here, or click to select files.")))
              ($ Stack
                 ($ Box {:height "300px" :width "300px"}
                    ($ Typography {:sx #js {:padding  "10px 10px 30px 10px"}
                                   :variant "h6" :component "h6"} "Files:")
                    (dom/ul
                      (-> (mapv #(do
                                   (dom/li {:key (j/get % :path)}
                                     (str (j/get % :name)" - " (-> % (j/get :size) size-string))))
                                accepted-files)
                          clj->js)))
                 ($ Box {:height "80px" :width "60px" :sx #js {:padding "10px 10px 10px 100px"}}
                    ($ Button {:onClick upload-files
                               :variant #_"outlined" "contained" :width "50px"}
                       "Upload")))))))))