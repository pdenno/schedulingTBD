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

(s/def :problem/domain keyword?)
(s/def ::goal seq?)
(s/def ::state (s/coll-of seq?))
(s/def ::domain-problem  (s/keys :req [:problem/domain] :req-un[::goal ::state]))

;;; ------ These concern out-bound on ws/send-to-chat. -------------------
(s/def :msg-text/string string?)
(s/def :msg-link/uri string?)
(s/def :msg-link/text string?)
(s/def ::msg-text-elem (s/keys :req [:msg-text/string]))
(s/def ::msg-link-elem (s/keys :req [:msg-link/uri :msg-link/text]))

(s/def ::chat-msg-vec (s/or
                       :something (s/and vector?
                                         #(every? (fn [elem]
                                                    (or (s/valid? ::msg-text-elem elem)
                                                        (s/valid? ::msg-link-elem elem)))
                                                  %))
                       :nothing nil?))

(defn out-bound-dispatch-key? [x] (#{:run-long           ; <=================================
                                     :clear-promise-keys ; Server tells you to forget a promise.
                                     :alive?             ; Server is asking whether you are alive.
                                     :reload-proj        ; Server created new current project (e.g. starting, surrogates).
                                     :ping-confirm       ; Server confirms your ping.
                                     :tbd-says} x))
(s/def ::client-id string?) ; ToDo: random-uuid once switch to transit.
(s/def ::dispatch-key out-bound-dispatch-key?)
(s/def ::chat-msg-obj (s/keys :req-un [::client-id ::dispatch-key] :opt-un [::chat-msg-vec]))
