(ns stbd-app.components.run-dialog
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [POST]]
   [promesa.core :as p]
   [applied-science.js-interop :as j]
   [helix.core :refer [defnc $]]
   [helix.dom :as dom]
   [helix.hooks :as hooks]
   ["@mui/icons-material/RunCircle$default" :as RunCircle]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Card$default" :as Card]
   ["@mui/material/Stack$default" :as Stack]
   ["@mui/material/Typography$default" :as Typography]
   ["@mui/material/Dialog$default" :as Dialog]
   ["@mui/material/IconButton$default" :as IconButton]
   [stbd-app.util   :refer [common-info]]
   [stbd-app.ws     :as ws]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

(def example-code
  "int : nProducts = 2;\nset of int: Product = 1..nProducts;\nenum Task = {nibManufacturing, barrelCapManufacturing, internalMechanisms, assembly, qualityControl, packaging};\narray [Product, Task] of int: taskDuration = [|1, 1, 1, 1, 1, 1|\n\t\t\t\t\t       1, 1, 1, 1, 1, 1|];\n\nint : minTime = 0;\n% It shouldn't take longer than doing only one thing at a time.\nint : maxTime = sum (p in Product, t in Task) (taskDuration[p, t]);\n\n% We assume a task uses one major resource; resources are synonymous with tasks.\narray [Product, Task] of var minTime..maxTime: taskStarts; % 'var' means it is a decision variable.\narray [Product, Task] of var minTime..maxTime: taskEnds;\n\n% This is used in the constraint below.\npredicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) = s1 + d1 <= s2 \\/ s2 + d2 <= s1;\n\n% ensure that two Products aren't using the same resource (task) at the same time.\nconstraint forall (t in Task)\n\t     (forall  (i,k in Product where i < k)\n\t\t(no_overlap (taskStarts[i,t], taskDuration[i,t], taskStarts[k,t], taskDuration[k,t])));\n\n% We assume no tasks are running now, so the first task of some product can start now.\nconstraint exists (p in Product) (taskStarts[p, nibManufacturing] = 0);\n\n% Starting some time in the past is not allowed.\nconstraint forall (p in Product where p > 1) (taskStarts[p, nibManufacturing] > 0);\n\n% Every other task can start after the previous task ends.\nconstraint forall (p in Product) (taskEnds[p, nibManufacturing] <= taskStarts[p, barrelCapManufacturing]);\nconstraint forall (p in Product) (taskEnds[p, barrelCapManufacturing] <= taskStarts[p, internalMechanisms]);\nconstraint forall (p in Product) (taskEnds[p, internalMechanisms] <= taskStarts[p, assembly]);\nconstraint forall (p in Product) (taskEnds[p, assembly] <= taskStarts[p, qualityControl]);\nconstraint forall (p in Product) (taskEnds[p, qualityControl] <= taskStarts[p, packaging]);\n\n% A task ends taskDuration time units after it starts.\nconstraint forall (p in Product, t in Task) (taskEnds[p, t] == taskStarts[p, t] + taskDuration[p, t]);\n\n% Minimize time elapsed when the final task ends (maxspan).\nsolve minimize max (p in Product) (taskEnds[p, packaging]);\n\n")

;;; https://mui.com/material-ui/react-modal/
(defnc RunDialog [{:keys [code]}]
  (let [[open, set-open] (hooks/use-state false)
        [state-text set-state-text]  (hooks/use-state "Running...")
        modal            (hooks/use-ref nil)]
    (letfn [(handle-close [] (set-open false))
            (run-success [_] (when (j/get modal :current)
                                ;;(set-open false)
                                (set-state-text "Saving to server succeeded.")))
            (run-failure [status status-text]
              (when (j/get modal :current)
                (log/info "Saving example failed: status = " status " status text = " status-text)
                (set-state-text "Communication with the server failed.")
                (set-open true)))
            (handle-run [& _args]
              (set-open true)
              (log/info "before http call.")
              (let [prom (p/deferred)]
                (POST (str "/api/run-minizinc?project-id=" (:project/id @common-info) "&client-id=" ws/client-id)
                      {:timeout 30000
                       :body {:code example-code}
                       :handler (fn [resp]
                                  (let [res (p/resolve! prom resp)]
                                    (log/info "Response from server = " res)
                                    res))
                       :error-handler (fn [{:keys [status status-text]}]
                                        (p/reject! prom (ex-info "CLJS-AJAX error on /api/run-minizinc"
                                                                 {:status status :status-text status-text})))})
                prom))]
      (dom/div {:ref modal}
        ($ IconButton {:onClick handle-run} ($ RunCircle))
        ($ Dialog  {:open open :onClose handle-close}
           ($ Stack {:direction "column"}
              ($ Box
                 ($ Card {:elevation 5}
                    ($ Typography {:sx #js {:padding  "40px 40px 30px 50px;"} ; top right bottom left
                                   :id "save-modal-title"
                                   :variant "h6"
                                   :component "h6"}
                       "Some text here")))))))))
