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
(defn outbound-dispatch-key? [x] (#{:clear-promise-keys        ; Server tells you to forget a promise.
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

(s/def ::client-id (s/or :normal string? :debug #(= % :console))) ; ToDo: random-uuid once switch to transit.
(s/def ::dispatch-key outbound-dispatch-key?)
(s/def ::text (s/and string? #(not-empty %)))
(s/def ::chat-msg-obj (s/and (s/keys :req-un [::client-id ::dispatch-key])
                             #(if (#{:sur-says :iviewr-says :update-code} (:dispatch-key %))
                                (s/valid? ::text (:text %))
                                true)))

(def agent-key?
  "These are what are in the schema."
  #{:agent/agent-id :agent/agent-type :agent/assistant-id :agent/base-type :agent/expertise
    :agent/llm-provider :agent/model-class :agent/pid :agent/response-format-path :agent/surrogate? :agent/instruction-path
    :agent/instruction-string :agent/thread-id :agent/timestamp :agent/tools :agent/vector-store-paths})

(defn agent-keys?
  "The keys of an agent object to be put in the DB include none that aren't in the schema."
  [obj] (every? agent-key? (keys obj)))

(s/def ::db-agent (s/and (s/keys :req [:agent/base-type :agent/agent-id :agent/agent-type]
                                 :opt [:agent/assistant-id :agent/expertise :agent/instruction-path :agent/instruction-string
                                       :agent/llm-provider :agent/model-class :agent/response-format-path :agent/surrogate?
                                       :agent/thread-id :agent/timestamp :agent/tools :agent/vector-store-paths])
                         #(agent-keys? %)))

(s/def :agent/base-type keyword?)
(s/def :agent/agent-id keyword?)
(s/def :agent/agent-type #{:shared-assistant :project :system})

(s/def :agent/assistant-id string?)
(s/def :agent/expertise string?)
(s/def :agent/instruction-path string?)
(s/def :agent/instruction-string string?)
(s/def :agent/llm-provider keyword?)
(s/def :agent/model-class keyword?)
(s/def :agent/response-format-path string?)
(s/def :agent/surrogate? boolean?)
(s/def :agent/thread-id string?)
(s/def :agent/timestamp inst?)
(s/def :agent/tools (s/or :dehydrated string? :hydrated (s/coll-of map? :kind vector?)))
(s/def :agent/vector-store-paths (s/coll-of string?))

;;;-------------------------------------------------- For interviewers and mock --------------------------------------------
;;; To check the structure of messages to and from the interviewer:
(s/def ::interviewer-msg (s/and (s/keys :req-un [:iviewr/message-type])
                                (fn [msg]
                                  (case (:message-type msg)
                                    :BACKCHANNEL-COMMUNICATION (s/valid? :iviewr/backchannel-coms msg) ; ToDo: "BACKCHANNEL-COMS"
                                    :SUPPLY-QUESTION (s/valid? :iviewr/supply-question msg)
                                    :QUESTION-TO-ASK (s/valid? :iviewr/question-to-ask msg)
                                    :INTERVIEWEES-RESPOND (s/valid? :iviewr/interviewees-respond msg)
                                    :DATA-STRUCTURE-REFINEMENT (s/valid? :iviewr/data-structure-refinement msg)
                                    :CONVERSATION-HISTORY (s/valid? :iviewr/conversation-history msg)
                                    :EADS-INSTRUCTIONS (s/valid? :iviewr/eads-instructions msg)
                                    :COURSE-CORRECTION (s/valid? :iviewr/course-correction msg)
                                    :STATUS (s/valid? :iviewr/status msg)))))

(s/def :iviewr/activity (s/coll-of :iviewr/activity-map :kind vector?))
(s/def :iviewr/activity-map (s/keys :req-un [:iviewr/question :iviewr/answer]))
(s/def :iviewr/answer string?)
(s/def :iviewr/backchannel-coms (s/keys :opt-un [:iviewr/question :iviewr/response :iviewr/advice]))
(s/def :iviewr/budget number?)
(s/def :iviewr/commit-notes string?)
(s/def :iviewr/conversation-history (s/keys :req-un [:iviewr/interviewee-type] :opt-un [:iviewr/activity :iviewr/budget :iviewr/data-structure :iviewr/EADS]))
(s/def :iviewr/course-correction (s/keys :opt-un [:iviewr/advice :iviewr/question]))
(s/def :iviewr/data-structure map?)
(s/def :iviewr/data-structure-refinement (s/keys :req-un [:iviewr/commit-notes :iviewr/data-structure]))
(s/def :iviewr/EADS (s/or :dehydrated string? :hydrated map?))
(s/def :iviewr/eads-instructions (s/keys :req-un [:iviewr/interview-objective :iviewr/EADS]))
(s/def :iviewr/interview-objective string?)
(s/def :iviewr/interviewees-respond (s/keys :req-un [:iviewr/response]))
(s/def :iviewr/interviewee-type #(#{:human :machine} %))
(s/def :iviewr/question string?)
(s/def :iviewr/question-to-ask (s/keys :req-un [:iviewr/question]))
(s/def :iviewr/response string?)
(s/def :iviewr/status #(string? (get % :status)))
(s/def :iviewr/supply-question (s/keys :req-un [:iviewr/budget]))
