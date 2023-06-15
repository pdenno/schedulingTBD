(ns stbd-app.rm-mode.parser
  (:require
   ["@lezer/highlight" :as highlight :refer [tags]]
   ["@codemirror/language" :as language :refer [LRLanguage LanguageSupport]]
   ["lezer-clojure" :as lezer-clj]
   [applied-science.js-interop :as j]
   [stbd-app.rm-mode.extensions.close-brackets :as close-brackets]
   [stbd-app.rm-mode.extensions.match-brackets :as match-brackets]
   [stbd-app.rm-mode.extensions.formatting :as format]
   [stbd-app.rm-mode.extensions.selection-history :as sel-history]
   [stbd-app.rm-mode.extensions.eval-region :as eval-region]
   [stbd-app.rm-mode.keymap :as keymap]
   [stbd-app.rm-mode.node :as n]
   [stbd-app.rm-mode.state :as state]))

(def fold-node-props
  (let [coll-span (fn [^js tree] #js{:from (inc (n/start tree))
                                     :to (dec (n/end tree))})]
    (j/lit
     {:Vector coll-span
      :Map coll-span
      :List coll-span})))

;;; clojure.grammar defines the keys used here.
;;; Therefore, perhaps my parse structure is not so far off that I couldn't use this.
;;; First step is to study the 'non-abtract syntax tree' (See https://lezer.codemirror.net/) produced for clojure.
;;; Problem: I don't see hooks on how to do that.
(def style-tags
  (clj->js {:NS (.-keyword tags)
            :DefLike (.-keyword tags)
            "Operator/Symbol" (.-keyword tags)
            "VarName/Symbol" (.definition tags (.-variableName tags))
            :Boolean (.-atom tags)
            "DocString/..." (.-emphasis tags)
            :Discard! (.-comment tags)
            :Number (.-number tags)
            :StringContent (.-string tags)
            ;; need to pass something, that returns " when being parsed as JSON
            ;; also #js doesn't treat this correctly, hence clj->js above
            "\"\\\"\"" (.-string tags)
            :Keyword (.-atom tags)
            :Nil (.-null tags)
            :LineComment (.-lineComment tags)
            :RegExp (.-regexp tags)}))

#_(defn parse ; Not used!
  "Wrapper over ev/processRM"
  [text]
  (try
    (ev/processRM :ptag/exp text)
    (catch :default _e "...")))

(def parser lezer-clj/parser)

;;; HEY, DON'T DELETE THIS QUITE YET!
#_(comment
  ;; to build a parser \""live" from a .grammar file,
  ;; rather than using a precompiled parser:
  (def parser
    (lg/buildParser
     (shadow.resource/inline "./clojure/clojure.grammar")
     #js{:externalProp n/node-prop})))

(defn syntax
  ([] (syntax parser))
  ([^js parser]
   (.define LRLanguage
            #js {:parser (.configure parser #js {:props #js [format/props
                                                             (.add language/foldNodeProp fold-node-props)
                                                             (highlight/styleTags style-tags)]})})))

(def ^js/Array complete-keymap keymap/complete)
(def ^js/Array builtin-keymap keymap/builtin)
(def ^js/Array paredit-keymap keymap/paredit)

(def default-extensions
  #js[(syntax lezer-clj/parser)    ;; <=== As it stands, this gives you clojure highlighting.
      (close-brackets/extension)   ;;      See https://lezer.codemirror.net/
      (match-brackets/extension)   ;;      I'd have to decide whether to use it. (Is my grammar even regular?)
      (sel-history/extension)      ;;      Or maybe see if I can do highlighing without anything like this.
      (format/ext-format-changed-lines)
      (eval-region/extension {:modifier "Alt"})])

(def language-support
  "Eases embedding clojure mode into other languages (e.g. markdown).
  See https://codemirror.net/docs/ref/#language.LanguageSupport for motivations"
  (LanguageSupport. (syntax) (.. default-extensions (slice 1))))

#_(comment

  (let [state (state/make-state #js[(syntax lezer-clj/parser)] "[] a")]
    (-> (n/tree state)
        (.resolve 2 1) ;; Symbol "a"
        .-prevSibling
        js/console.log))

  (let [state (state/make-state #js[(syntax lezer-clj/parser)] "\"\" :a")]
    (-> state
        n/tree
        (n/cursor 0 1)
        ))
  (let [state (state/make-state #js[(syntax lezer-clj/parser)] "a\n\nb")]
    (-> state
        (n/tree 1 1)
        (->> (n/string state))
        str
        ))
  (let [state (state/make-state #js[(syntax lezer-clj/parser)] "([]| s)")]
    (-> state
        n/tree
        (n/terminal-cursor 3 1)
        ))

  (let [state (state/make-state #js[(syntax lezer-clj/parser)] "(|")]
    (-> state
        (close-brackets/handle-close ")")
        (->> (n/string state)))))
