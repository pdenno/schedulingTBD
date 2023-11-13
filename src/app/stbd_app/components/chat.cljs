(ns stbd-app.components.chat
  "This is used pop up a model indicating the URL at which the example can be retrieved."
  (:require
   [ajax.core :refer [GET POST]]
   [applied-science.js-interop :as j]
   [cljs.reader                :refer [read-string]]
   [clojure.edn                :as edn]
   [clojure.walk               :as walk :refer [keywordize-keys]]
   [helix.core                 :refer [defnc $]]
   [helix.hooks                :as hooks]
   [promesa.core               :as p]
   ["@mui/icons-material/Send$default" :as Send]
   ["@mui/material/Box$default" :as Box]
   ["@mui/material/Button$default" :as Button]
   ["@mui/material/IconButton$default" :as IconButton]
   ["@mui/material/LinearProgress$default" :as LinearProgress]
   ["@mui/material/Link$default" :as Link]
   ["@mui/material/Stack$default" :as Stack]
   ["react-chat-elements/dist/main"    :as rce]
   [scheduling-tbd.util :as sutil :refer [timeout-info #_invalidate-timeout-info]]
   [stbd-app.util :as util]
   [taoensso.timbre :as log :refer-macros [info debug log]]))

(def diag (atom nil))

(def progress-handle
  "The thing that can be called by js/window.clearInterval to stop incrementing progress under js/window.setInterval."
  (atom nil))

(def progress-atm "Percent allowed duration for eval-cell. 100% is a timeout." (atom 0))

(defn compute-progress
  "Use either progress-atm or timeout-info to return a percent done."
  []
  (let [now (.getTime (js/Date.))
        info @timeout-info
        timeout-at (:timeout-at info)
        res (if (:valid? info)
              (if (> now timeout-at)
                100
                (int (* 100.0  (- 1.0 (double (/ (- timeout-at now) (:max-millis info)))))))
              (+ @progress-atm 2))]
    res))

(def white-style (clj->js {:color "background.paper"}))
(def blue-style (clj->js {:color "primary"}))

(defn register
  [name elem]
  (swap! util/component-refs #(assoc % name elem))
  elem)

(defn add-msg [msg-list msg]
  (-> msg-list
      js->clj
      (conj msg)
      clj->js))

(def item-keys "Atom for a unique :key of some UI object." (atom 0))

(defn make-link
  "Create a MUI link. (Works for react-chat-elements too.)
   Example usage: (make-link {:href 'https://en.wikipedia.org/wiki/%22Hello,_World!%22_program' :text 'Hello, world!'})"
  [{:keys [href text]}]
  ($ Link {:key (swap! item-keys inc)
           :href href}
     text))

(defnc Chat [{:keys [height conversation]}]
  (let [[msg-list set-msg-list] (hooks/use-state conversation)
        [progress set-progress] (hooks/use-state 0)
        [progressing? _set-progressing] (hooks/use-state false)
        [user-text set-user-text] (hooks/use-state "")
        input-ref (hooks/use-ref nil)]
    (hooks/use-effect [progressing?]
      (reset! progress-atm 0)
      (reset! progress-handle
              (js/window.setInterval
               (fn []
                 (let [percent (compute-progress)]
                   (if (or (>= progress 100) (not progressing?))
                     (do (set-progress 0) (js/window.clearInterval @progress-handle))
                     (set-progress (reset! progress-atm percent)))))
               200)))
    (hooks/use-effect [user-text]
       (when (not-empty user-text)
         (let [prom (p/deferred)]
           (POST "/api/user-says"
                 {:params {:user-text user-text}
                  :timeout 30000
                  :handler (fn [resp] (p/resolve! prom resp))
                  :error-handler (fn [{:keys [status status-text]}]
                                   (p/reject! prom (ex-info "CLJS-AJAX error on /api/user-says" {:status status :status-text status-text})))})
           (-> prom
               (p/then #(set-msg-list (add-msg msg-list {:type "text" :text (:message/text %) :title "TBD" :titleColor "Red"})))
               (p/catch #(log/info (str "CLJS-AJAX user-says error: status = " %)))))))
    ;;(log/info "chat height = " height)
    ;;(reset! diag {:height height})
    ($ Stack {:direction "column" :spacing "0px"}
       ($ Box {:sx (clj->js {:overflowY "auto" ; :sx was :style
                             :display "flex"
                             :flexGrow 1
                             :maxHeight 300 ; (- height 100)
                             :flexDirection "column"})}
          ($ rce/MessageList {:dataSource msg-list}))
       ($ LinearProgress {:variant "determinate" :value progress})
       ($ Stack {:direction "row" :spacing "0px"}
          ($ rce/Input {:referance input-ref ; <==== Yes, rilly!
                        :value user-text
                        :placeholder "Type here..."
                        :multiline true})
          ($ IconButton {:onClick #(when-let [iref (j/get input-ref :current)]
                                     (when-let [text (not-empty (j/get iref :value))]
                                       (set-msg-list (add-msg msg-list  {:type "text" :text text
                                                                         :title "You" :color "Green" :position "right"}))
                                       (j/assoc! iref :value "")
                                       (set-user-text text)))}
             ($ Send))))))

(defn get-children
  "Return a vector of the children of an HTMLCollection."
  [obj]
  (when (= "HTMLCollection" (j/get-in obj [:constructor :name]))  ;; HANDY! Get the string naming the type.
    (let [res (atom [])]
      (doseq [ix (range (j/get obj :length))]
        (swap! res conj (j/get obj ix)))
      @res)))

(defn find-elem
  "Search the DOM for a node passing the argument test."
  [node test]
  (let [found? (atom false)
        cnt (atom 0)]
    (letfn [(fe [obj]
              (swap! cnt inc)
              (let [typ (j/get-in obj [:constructor :name])
                    chiln?  (j/get obj :children)]
                (log/info "typ = " typ " obj = " obj)
                (cond @found?                  found?
                      (> @cnt 50)              :failure
                      (test obj)               (reset! found? obj)
                      (vector? obj)            (map fe obj)
                      (nil? obj)               found?
                      chiln?                   (fe chiln?)
                      (= "HTMLCollection" typ) (->> obj get-children fe))))]
      (fe node)
      @found?)))

(defn get-conversation
  "Return a promise that will resolve to the vector of a maps describing the complete conversation so far."
  [project-id]
  (let [prom (p/deferred)]
    (GET (str "/api/get-conversation?project-id=" project-id)
         {:timeout 3000
          :handler (fn [resp] (p/resolve! prom resp))
          :error-handler (fn [{:keys [status status-text]}]
                           (p/reject! prom (ex-info "CLJS-AJAX error on /api/list-projects"
                                                    {:status status :status-text status-text})))})
    prom))

(def example-converse
  [{"message/from" "system",
    "message/id" 0,
    "message/text" "{:text/vec [\"Describe your scheduling problem in a few sentences or \"\n    {:link-info {:href \"http://localhost:3300/learn-more\"\n                 :text \"learn more about how this works\"}}\n   \".\"]",
    "message/time" "2023-11-11T10:12:49Z"},
   {"message/from" "user",
    "message/id" 1,
    "message/text" "We are a medium-sized craft beer brewery...."
    "message/time" "2023-11-11T10:12:49Z"},
   {"message/from" "system",
    "message/id" 2,
    "message/text" "Great! We'll call your project 'craft-beer-brewery-scheduling'. ",
    "message/time" "2023-11-11T10:12:49Z"}])

;;; This is a bit weird because read-string is only the right thing to do when
(defn msg-text2rce-string
  "Argument is a DB :message/text, which can read-string to a vector that contains a mix of text and
   maps with :link-info in them.
   Return a JS array containing content that could be the :text of a react-chat-elements/message."
  [msg-text]
  (letfn [(pfs [x]
            (cond (vector? x) (mapv pfs x)
                  (and (map? x) (contains? x :link-info)) (-> x :link-info make-link)
                  :else x))]
    (try (-> msg-text pfs)
         (catch :default e (log/info "Bad DB :message/text: " {:msg-text msg-text :err e})))))

(defn conversation2rce
  "Rewrite the conversation messages to objects acceptable to the React Chat Elements component."
  [msgs]
  (let [msgs  (walk/keywordize-keys msgs)
        who   {"system" "TBD", "user" "YOU"}
        color {"system" "Red", "user" "Green"}]
    (reset! diag msgs)
    (->> msgs
         (mapv #(-> {:type "text"}
                    (assoc :text (-> % :message/text msg-text2rce-string))
                    (assoc :title (get who (:message/from %)))
                    (assoc :titleCollor (get color (:message/from %)))))
         clj->js)))
