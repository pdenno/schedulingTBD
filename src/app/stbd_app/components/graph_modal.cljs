(ns stbd-app.components.graph-modal
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [POST #_raw-response-format]]
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   ["@chatscope/chat-ui-kit-react/dist/cjs/Buttons$default" :refer [AttachmentButton]]
   [helix.core :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   [stbd-app.components.graph :refer [GraphPane]]
   ["react-dropzone/dist/index$useDropzone" :as useDropzone] ; This worked with 4.2.3, and not some versions afterwards.
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/Card$default" :as Card]
   ["@mui/material/Divider$default" :as Divider]
   ["@mui/material/Input$default" :as Input]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/Typography$default" :as Typography]
   ["@mui/material/Dialog$default" :as Dialog]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/icons-material/TableChart$default" :as TableChart]
   ["@mui/icons-material/Polyline$default" :as Polyline]
   [stbd-app.util     :refer [common-info]]
   [stbd-app.ws       :as ws]
   [taoensso.telemere :refer [log!]]))

(def ^:diag diag (atom nil))

(defn size-string
  [size]
  (cond (< size 1024)         (str size " b")
        (< 1024 size 1048576) (str (int (/ size 1024)) " k")
        :else                 (str (-> size (/ 1048576) int) "."
                                   (-> (/ (rem size 1048576) 1048577) (* 100) int) " M")))

(def drag-text "Drag and drop a file here, or click to select files.")

;;; https://mui.com/material-ui/react-modal/
;;; https://react-dropzone.js.org/#section-basic-example
(defnc GraphModal [{:keys [mid]}] ;mid is message-id, i don't know if there's a better way to find specific EADSes as they evolve
  (let [[open, set-open] (hooks/use-state false)
        modal           (hooks/use-ref nil)]
    (letfn [(show-graph []
              (when (j/get modal :current) (set-open true)))
            (handle-close [] (set-open false))]
      (dom/div {:ref modal}
               ($ IconButton {:onClick show-graph} ($ Polyline))
        ($ Dialog  {:open open :onClose handle-close}
           ($ Stack {:direction "row"
                     :divider ($ Divider {:variant "activeVert" :color "black"})} ; ToDo: only "activeVert" works.
              ($ GraphPane {:init-graph "graph TD\nA[Client] --> B[Load Balancer]\nB --> C[Server01]\nB --> D[Server02]"})))))))
