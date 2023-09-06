# schedulingTBD

## Introduction

[The following is cut from a workshop talk proposal. Apologies!]

Can we effectively engage a person possessing little background in model formulation in a “conversation”
where we jointly (human/AI) formulate a model important to their work? NIST’s Engineering Laboratory is
initiating a project ”Human/Machine Teaming for Manufacturing Digital Twins” to explore this research question.
Our goal is to foster measurement science, standards, and innovation in human/AI teaming on cognitive tasks
in engineering and production. Our earlier work [1, 2] focused specifically on teaming to formulate scheduling
problems in MiniZinc, a domain-specific language (DSL) for combinatorial optimization. That work piloted a
tool integrating software for Bayes net-based plan recognition [3] and a metamodel of the MiniZinc DSL to help
users develop production scheduling systems in Jupyter notebooks. Our current work seeks to establish a
measurement science for joint work with such models. We believe our work formulating scheduling problems
provides an on-ramp to more challenging teaming tasks with digital twins [4].

Establishing common ground between human and machine agents is a key challenge in joint cognitive
work (JCW) and a precursor to requirements engineering. In analogy, establishing common ground is what
a professor might do during office hours. Suppose the professor teaches a course in formulating production
scheduling systems with MiniZinc. As an expert user of the tool, the professor’s first challenge is to quickly
understand what problem the student is trying to solve with MiniZinc. With a classification of the problem in
mind, the professor may recognize the plan of the student’s attempt, get the student started on a plan, or
remedy mistakes in the student’s plan.

Towards establishing a measurement science for joint work with models, our current work seeks a generic
architecture for human/AI teaming for formulation with DSLs. We are developing a tool to author and capture
results from teaming sessions. The authoring capabilities provided to experts in use of the analytical tool
(and its DSL) includes (a) authoring few-shot prompts to large language models (LLMs) to analyze the user’s
contribution to the conversation, and (b) authoring plan steps to an AI planner [5] that directs the conversation
towards establishing common ground and achieving formulation goals. As in the earlier work, we are using
MiniZinc and scheduling problems as a test case, but, with time, we intend to investigate Modelica and process
problems.

We have identified three kinds of analysis of user responses to pursue with help from the LLM: (1) user
problem domain, (2) solution technology domain, and (3) metatheoretical. Problem domain analysis can be
augmented by background prompts to the LLM API to gain knowledge of the problem domain. Solution tech-
nology domain analysis can walk the user through the application of techniques such as Object Role Model-
ing [6] to link data to constructs of the solution technology. Metatheoretical analysis of user sentences includes
classification of (i) speech act type and propositional attitude, (ii) topic (e.g. causation, objective, preferences,
constraints, etc.) (iii) explanatory goal (e.g. mechanistic [7, 8], interventionist [9], unificationist [10]) and, (iv)
how the sentence bridges theory, model, kind, object, and property [11]. Together, use of these three analysis
kinds provides input to AI planning of the next machine agent query in the conversation.

By using the tool to collect experiences with various techniques authored, we hope to learn what works
and what does not. This provides small, early steps towards the measurement science sought. The ability
to produce valid formulation in the DSL from informally stated user requirements is an obvious measure of
effectiveness. Other measures may include 1) whether the classification types enumerated above are effective
in directing conversation towards formulation, 2) how effectively AI planning can direct the conversation towards
formulation, 3) how effective AI planning is at assessing completion of the model, 4) what can be learned by
running the code produced, or parts of it, 5) metrics for effective explanation, and 6) the value of reference to
ontologies of related concepts in the conversation.

![alt text](https://github.com/pdenno/schedulingTBD/blob/main/doc/SchedulingTBD-early.png?raw=true)
