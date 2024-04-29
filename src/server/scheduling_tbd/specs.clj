(ns scheduling-tbd.specs
  "This provides Clojure specs used throughout the project."
  (:require
   [clojure.spec.alpha           :as s]))

(s/def ::positive-proposition
  #(and (seq? %)
        (-> % first symbol?)
        (not-any? coll? %))) ; There are not functional terms allowed here, etc.

(s/def ::ground-positive-proposition
  (s/and ::positive-proposition
         #(not-any? (fn [role] (and (symbol? role) (= "?" (subs (name role) 0 1))))
                    (rest %))))

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

;;; ToDo: I need to be able to push an update to the conversation!
(defn outbound-dispatch-key? [x] (#{:run-long             ; diagnostic
                                    :clear-promise-keys   ; Server tells you to forget a promise.
                                    :alive?               ; Server is asking whether you are alive.
                                    :load-proj            ; Make a request to restart the project, including doing a resume-conversation
                                    :request-conversation ; Like :load-proj but don't do a resume-conversation.
                                    :ping-confirm         ; Server confirms your ping.
                                    :sur-says
                                    :tbd-says} x))
(s/def ::client-id string?) ; ToDo: random-uuid once switch to transit.
(s/def ::dispatch-key outbound-dispatch-key?)
(s/def ::chat-msg-obj (s/keys :req-un [::client-id ::dispatch-key] :opt-un [::chat-msg-vec]))
