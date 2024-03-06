(ns stbd-app.util
  (:require
   #_[applied-science.js-interop :as j]
   #_[taoensso.timbre :as log :refer-macros [info debug log]]))

(def ^:diag diag (atom nil))

(def server-port 3300) ; ToDo: Get this from config.

;;; ToDo: Is there a react way? It looks like react doesn't have this notion.
(def ^:diag root "The application's root 'Symbol(react.element)' element" (atom nil))

#_(defn fn? [arg] ; I didn't see this in the docs!
  (= "Function" (j/get-in arg [:constructor :name])))

;;; ToDo: Is there are idiomatic React way to do this? (Find way around structure.)
(def component-refs
  "Some components instances are named and their refs stored here."
  (atom  {}))
