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
  #(and (seq? %)
        (= 'not (first %))
        (-> % second seq?)
        (-> % second first symbol?)
        (not-any? coll? (second %))))

(s/def ::proposition
  (s/or :positive ::positive-proposition
        :negated  ::negated-proposition))


;;; ------ These concern out-bound on ws/send-to-chat. -------------------
;;; ToDo: I need to be able to push an update to the conversation!
(defn outbound-dispatch-key? [x] (#{:run-long                  ; diagnostic
                                    :clear-promise-keys        ; Server tells you to forget a promise.
                                    :alive?                    ; Server is asking whether you are alive.
                                    :load-graph                ; Load a graph into the 4th quadrant.
                                    :load-table                ; Load a table into the 4th quadrant.
                                    :load-proj                 ; Make a request to restart the project, including doing a resume-conversation
                                    :update-conversation-text  ; Like :load-proj but don't do a resume-conversation.
                                    :ping-confirm              ; Server confirms your ping.
                                    :interviewer-busy?         ; Tells client to prevent changing the conversation/planning domain.
                                    :sur-says
                                    :iviewr-says
                                    :update-code} x))

(s/def ::client-id string?) ; ToDo: random-uuid once switch to transit.
(s/def ::dispatch-key outbound-dispatch-key?)
(s/def ::text (s/and string? #(not-empty %)))
(s/def ::chat-msg-obj (s/and (s/keys :req-un [::client-id ::dispatch-key])
                             #(if (#{:sur-says :iviewr-says :update-code} (:dispatch-key %))
                                (s/valid? ::text (:text %))
                                true)))
