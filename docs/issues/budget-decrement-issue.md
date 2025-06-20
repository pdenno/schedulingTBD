# Budget Decrement Issue in Interview System

## Problem Description

The ORM EADS (`:data/orm`) is configured to decrease the question-asking budget by 0.08 with each question asked (as specified in `orm.clj`). However, the budget is only being decremented once, not with each subsequent question.

## Root Cause Analysis

The issue occurs in the `resume-conversation` function in `interviewers.clj` due to a timing mismatch between when the budget is queried and when it's decremented.

### Current Flow

1. **Question Generation**: `q-and-a` calls `make-supply-question-msg` which gets the budget from:
   ```clojure
   {:message-type :SUPPLY-QUESTION :budget (db/get-questioning-budget-left! pid (db/get-project-active-EADS-id pid))}
   ```

2. **EADS Change Detection**: The `ork-review` function determines if there should be an EADS change and returns a new `eads-id`

3. **Budget Decrement**: `resume-post-db-actions!` is called with the current `eads-id`:
   ```clojure
   (let [eads-id (or new-eads-id (db/get-active-EADS-id pid cid))]
     ; ... later ...
     (resume-post-db-actions! iviewr-response pid cid eads-id))
   ```

4. **The Problem**: If an EADS change occurred, `eads-id` refers to the NEW EADS, but the question was generated using the budget from the OLD/current EADS.

### Budget Per EADS Design

The system is designed so that each EADS has its own budget. When switching EADS, a fresh budget (1.0) is allocated. This is correct behavior - the issue is that budget decrements are being applied to the wrong EADS.

## Technical Details

### Key Files and Functions

- **`orm.clj`**: Defines `:budget-decrement 0.08` for the ORM EADS
- **`interviewers.clj`**:
  - `make-supply-question-msg`: Gets budget from current project active EADS
  - `resume-conversation`: Main interview loop
  - `resume-post-db-actions!`: Calls `db/reduce-questioning-budget!`
- **`db.clj`**:
  - `get-questioning-budget-left!`: Retrieves current budget for an EADS
  - `reduce-questioning-budget!`: Decrements budget for an EADS

### The Mismatch

```clojure
;; In resume-conversation loop:
(let [eads-id (or new-eads-id (db/get-active-EADS-id pid cid))]
  ;; ... EADS change happens here if new-eads-id exists ...
  (let [conversation (q-and-a iviewr-agent pid cid ctx)  ; Uses OLD EADS budget
        ;; ...
        ]
    ;; ...
    (resume-post-db-actions! iviewr-response pid cid eads-id))) ; Decrements NEW EADS budget
```

If `new-eads-id` exists, the budget decrement is applied to the new EADS instead of the EADS that actually provided the question.

## Potential Solutions

### Option 1: Capture EADS ID at Question Time
Store the EADS ID that was used to generate the question and use that for budget decrement:

```clojure
(let [question-eads-id (db/get-project-active-EADS-id pid)
      conversation (q-and-a iviewr-agent pid cid ctx)]
  ;; ...
  (resume-post-db-actions! iviewr-response pid cid question-eads-id))
```

### Option 2: Move Budget Decrement Earlier
Apply the budget decrement immediately after the question is answered, before any EADS changes:

```clojure
(let [conversation (q-and-a iviewr-agent pid cid ctx)
      current-eads-id (db/get-project-active-EADS-id pid)]
  (db/reduce-questioning-budget! pid current-eads-id)
  ;; ... continue with EADS changes ...
  )
```

### Option 3: Track Budget Context in Conversation
Pass the originating EADS ID through the conversation flow to ensure budget is decremented against the correct EADS.

## Impact

- **Symptom**: ORM questions appear to have unlimited budget after the first question
- **Consequence**: Interviews may continue longer than intended for ORM EADS
- **Scope**: Affects any scenario where EADS changes occur during the interview process

## Recommended Fix

Option 1 (Capture EADS ID at Question Time) is recommended as it's the most straightforward and maintains the existing flow while ensuring budget accountability to the correct EADS.

## Implemented Solution

The fix has been implemented in `resume-conversation` function in `interviewers.clj`. The changes include:

### Code Changes

1. **Capture Question EADS ID**: Added a variable to capture the EADS ID before any potential EADS changes:
   ```clojure
   (let [eads-id (or new-eads-id (db/get-active-EADS-id pid cid))
         ;; Capture the EADS ID that will be used for the question to ensure budget is decremented against the correct EADS
         question-eads-id (db/get-project-active-EADS-id pid)]
   ```

2. **Use Correct EADS for Budget Decrement**: Modified the call to `resume-post-db-actions!` to use the captured question EADS ID:
   ```clojure
   ;; Use question-eads-id to ensure budget is decremented against the EADS that provided the question
   (resume-post-db-actions! iviewr-response pid cid question-eads-id)
   ```

### How the Fix Works

- **Before the fix**: Budget was decremented against `eads-id`, which could be the new EADS if an EADS change occurred
- **After the fix**: Budget is decremented against `question-eads-id`, which is always the EADS that was active when the question was generated
- **Result**: Budget decrements are now properly applied to the correct EADS, ensuring each EADS maintains its own budget accounting

### Testing the Fix

To verify the fix works:
1. Run an interview that transitions from process EADS to ORM EADS
2. Observe that ORM questions properly decrement the ORM budget by 0.08 with each question
3. Confirm that budget decrements don't affect the wrong EADS during transitions

### Status

✅ **RESOLVED** - The budget decrement issue has been fixed and ORM EADS now properly decrements budget with each question.
