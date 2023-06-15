(ns stbd-app.rm-mode.state
  (:require ["@codemirror/state" :as cm-state
             :refer [EditorState EditorSelection Extension StateCommand
                     ChangeSet ChangeDesc TransactionSpec StrictTransactionSpec]]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [stbd-app.rm-mode.extensions.formatting :as format]
            [taoensso.timbre :as log :refer-macros [info debug log]]))

;; (de)serialize cursors| and <selections> for testing

;;; Original
#_(defn make-state [extensions doc]
  ;; In RM doc returns here is doc input and ranges = []. So this does nothing.
  (let [[doc ranges] (->> (re-seq #"\||<[^>]*?>|[^<>|]+" doc)
                          (reduce (fn [[^string doc ranges] match]
                                    (cond (= match "|")
                                          [doc (conj ranges (.cursor EditorSelection (count doc)))]

                                          (str/starts-with? match "<")
                                          [(str doc (subs match 1 (dec (count match))))
                                           (conj ranges (.range EditorSelection
                                                                (count doc)
                                                                (+ (count doc) (- (count match) 2))))]
                                          :else
                                          [(str doc match) ranges])) ["" []]))]
    (log/info "3333333333333333 ranges = " ranges)
    ;; https://www.programcreek.com/typescript/?api=@codemirror/view.EditorView (search for new EditorView)
    (.create EditorState
             #js{:doc doc
                 :selection (if (seq ranges)
                              (.create EditorSelection (to-array ranges))
                              js/undefined)
                 :extensions (cond-> #js[(.. EditorState -allowMultipleSelections (of true))]
                               extensions
                               (j/push! extensions))})))

(defn make-state [extensions doc]
    ;; https://www.programcreek.com/typescript/?api=@codemirror/view.EditorView (search for new EditorView)
    (.create EditorState
             #js{:doc doc
                 :selection js/undefined
                 :extensions (cond-> #js[(.. EditorState -allowMultipleSelections (of true))]
                               extensions
                               (j/push! extensions))}))


(defn state-str [^js state]
  (let [doc (str (.-doc state))]
    (->> (.. state -selection -ranges)
         reverse
         (reduce (j/fn [doc ^:js {:keys [empty from to]}]
                   (if empty
                     (str (subs doc 0 from) "|" (subs doc from))
                     (str (subs doc 0 from) "<" (subs doc from to) ">" (subs doc to)))) doc))))

(defn apply-cmd [extensions cmd doc]
  (let [state (make-state extensions doc)
        !tr (atom nil)
        _ (cmd #js{:state state
                   :dispatch #(reset! !tr %)})
        tr @!tr]
    (state-str (j/get tr :state))))

(defn apply-f [extensions cmd doc]
  {:pre [(array? extensions)
         (fn? cmd)
         (string? doc)]}
  (let [state (make-state extensions doc)
        tr (cmd state)]
    (state-str (if tr (.-state tr) state))))
