(ns scheduling-tbd.specs
  "This provides Clojure specs used throughout the project."
  (:require
   [clojure.spec.alpha           :as s]))

(s/def ::positive-proposition
  #(and (seq? %)
        (-> % first symbol?)
        (not-any? coll? %))) ; There are not functional terms allowed here, etc.

(s/def ::negated-proposition
  #(and (= 'not (first %))
        (-> % second seq?)
        (-> % second first symbol?)
        (not-any? coll? (second %))))

(s/def ::proposition
  (s/or :positive ::positive-proposition
        :negated  ::negated-proposition))

(s/def :edits/add    (s/and set? (s/coll-of ::proposition)))
(s/def :edits/delete (s/and set? (s/coll-of ::proposition)))

(s/def :step/operator keyword?)
(s/def :step/args coll?)

(s/def ::plan-step (s/keys :req-un [:step/operator :step/args]))
(s/def ::state-edits (s/keys :req-un [:edits/add :edits/delete]))

(s/def :problem/name string?)
(s/def :problem/domain keyword?)
(s/def :problem/goal-string string?)
(s/def :problem/state-string string?)
(s/def ::domain-problem  (s/keys :req [:problem/ename :problem/domain :problem/goal-string :problem/state-string]))

;;; ToDo: This can go away with shop.
(s/def ::shop-obj-plan (s/and vector?
                              #(every? (fn [step] (s/valid? ::shop-step step)) %)))
(s/def ::shop-step (s/keys :req-un [::op ::cost]))

(s/def :msg-text/string string?)
(s/def :msg-link/uri string?)
(s/def :msg-link/text string?)
(s/def ::msg-text-elem (s/keys :req [:msg-text/string]))
(s/def ::msg-link-elem (s/keys :req [:msg-link/uri :msg-link/text]))
(s/def ::chat-msg-vec (s/and vector?
                             #(every? (fn [elem]
                                        (or (s/valid? ::msg-text-elem elem)
                                            (s/valid? ::msg-link-elem elem)))
                                      %)))

(s/def ::client-id string?)
(s/def ::msg-vec ::chat-msg-vec)
(s/def ::dispatch-key keyword?)
(s/def ::chat-msg-obj (s/keys :req-un [::msg-vec ::client-id ::dispatch-key]))
