# Plan: Skyve driven by agents over MCP and API

## Summary

This plan makes a Skyve application drivable by AI agents: Skyve's own cockpit hooked to an agentic service, and
third-party agents. It also brings Skyve Flows (long-running, human workflow) and behaviours into Skyve as first-class
metadata, because agents, flows and behaviours turn out to share one statement language and one identity model.

There are five parts.

1. **A conversation engine with no browser.** An agent opens a view, reads it, sets values, runs actions and saves,
   through the same conversation, view rules and security as a person. Every existing Skyve application becomes
   agent-drivable with no new metadata.
2. **Tasks.** XML that declares a short business task with its inputs up front ("fill in my timesheet from these
   entries", "invoice the uninvoiced time for this agreement"), so an agent does it in one call. A task lives in the
   system definition or in the database, where a user can save one from what they just did and share it.
3. **Processes.** The Skyve Flows model (a process of activities across people and time) as Skyve metadata, loadable
   from files and from the database, with an agent able to be the actor of an activity.
4. **An ontology.** XML that tells an agent what the metadata cannot: business meaning, rules that live in Java, and
   how records map to other systems.
5. **Mining.** Tooling that drafts tasks, processes and ontology from the metadata, a copy of the database and its
   audits.

The three executable things are layers that share one language:

| Layer | What it is | Lifetime | Who runs it |
|---|---|---|---|
| **Process** | A tree of activities on a subject document, triggered by data | Days, persisted | Several people, agents and automatic steps |
| **Task** | A unit of work with declared inputs and outputs | Short-lived | One agent, one person, or the system |
| **Behaviour statements** | `if`, `set`, `forEach` and the rest | Within a step | Whatever hosts them |

One service layer in Skyve serves agents, with MCP and REST as thin transports.

The plan is a programme, not one milestone. Phase 0 is specified closely enough to execute. Each later phase gets its
own `docs/ai/plan_m{n}.md` when it is scheduled.

A throwaway prototype was built on 25 November 2026 and run against Biz Hub Apps. It drove the real application through
the SAIL browser harness and interpreted a small task language. Its results are the evidence for much of the design
below; see [What the prototype showed](#what-the-prototype-showed).

## Scope

### Direction from Mike (discussion, 23 to 26 November 2026)

1. **Two strata.** One drives the existing views through conversations. The other is a definition that exposes all of
   its data up front and may reuse existing actions.
2. **An ontology** would help agents with structure and data, including use across system boundaries and the APIs
   available to a system.
3. **These definitions are XML**, like the rest of Skyve's metadata.
4. **AI should be able to draft them** from the existing metadata and a copy of the database with its audits.
5. **The cockpit drives itself through MCP**, hooked to an agentic service, and third-party agents use the same
   surface.
6. **Reuse the behaviour statements.** Behaviours already have an `<if/>`; integrate the two ideas.
7. **Typed, domain-centric operations.** Execution should use strongly typed operations such as the behaviour `<set/>`,
   to remove type coercion ambiguity, and still run off the views.
8. **Behaviour and workflow become Skyve metadata** as well as being first-class at runtime. Skyve Flows
   (`../skyve-flows`) has user-defined workflows today, and there is overlap to resolve.

### Decisions confirmed by Mike (26 November 2026)

9. **One model, two stores.** A process is XML metadata too. A process defined at runtime in the database is a second
   source of the same model, not a different thing. The same holds for behaviours.
10. **An agent can be the actor of an activity** in a process, as a person is.
11. **An automatic step runs as the process**, not as the user whose save triggered it and not as a super user.
    - A runtime-defined process is granted roles, the way a user or group is, by someone who may assign roles. It
      starts with none. Document and action permissions are checked against those roles.
    - A process defined in metadata files is trusted like Bizlet code and runs without that restriction: a file is
      deployed code, a database row is user content.
    - The audit records "process X, triggered by user U".
12. **Runtime definitions are revalidated**, at four points with one validator: when edited, in the build before a
    deployment, at startup, and when executed. See [Validation of runtime definitions](#validation-of-runtime-definitions).
13. **An invalid process either holds or skips**, as the process says. The default is to hold for a process that
    contains an approval or an acknowledgement, and to skip otherwise.

### Decisions proposed in this plan (need Mike's confirmation)

14. **"Task" is the name for stratum 2 definitions.** Skyve Flows already uses Process, Flow and Activity, and SAIL
    uses Workflow for several scripts run as one test run.
15. **The view is the guard, not the surface.** Each operation by a person or an agent is checked against the view as
    applied to the current bean, then applied to the bean as a typed value. See [The view guard](#the-view-guard).
16. **The guard belongs to the host, not the statement.** A `set` in a task acts for a user and goes through the view.
    A `set` in an automatic step of a process is automation and is checked by permissions on the bean.
17. **Engine first.** Build the conversation engine before any new XML, because it serves every existing application.
18. **One service layer, two transports.** Protocol-neutral Java, with MCP and REST/OpenAPI over it. Not built on
    `RestService`, whose insert, update and delete routes check no document permissions.
19. **An `agent` UX/UI** in the router, so a view can be specialised or withheld for agents with the existing `<uxui>`
    mechanism.
20. **Exposure is on by default** once the feature is enabled for an application, since the security model is the
    user's own. An application can withhold a view through the `agent` UX/UI.
21. **Tasks are short and deterministic.** Judgement (reading a receipt, parsing a note) stays with the calling agent.
    Anything that waits for a person, or runs for days, is a process.
22. **Three kinds of task implementation**: declared steps, a Java class, or a playbook (prose guidance an agent
    follows with the stratum 1 tools).
23. **Lookups never guess.** Finding a record is a read that returns candidates. Setting an association takes an
    identity, checked against the lookup's query.
24. **The ontology is an overlay** on documents, generated where possible and validated by `generateDomain`. Not OWL or
    RDF. Lifecycles come from process definitions where they exist.
25. **Audits record more.** Add the action name, the conversation id and who acted for whom, so that mining and
    accountability both work.

### Further direction from Mike (note to colleagues, "AI in Skyve", 26 November 2026)

26. **The cockpit has several components**: AI chat; in-app contextual messaging for teams; notifications and
    reminders; and the flow inbox.
27. **An ad hoc request through the cockpit chat leaves the Skyve UI in a filled-out but unsaved view.** AI may also
    guide a user through the system.
28. **Tasks are short-lived.** They are not promised to be atomic or transactional (Mike dropped both words).
29. **Tasks live in the system definition or in the database**, and are shareable between users.
30. **Mining can happen on the fly**: save what was just done as a task, share it with others, and connect it to
    system-defined or database processes.
31. **People run tasks too**, in a generic UI driven by the task's contract. A human step of a process may be
    implemented by a task, which asks for all its inputs up front in a dynamic UI. An agent's step may be implemented
    by a task as well.
32. **The ontology is a data mesh layer above the domain model**, needed mainly for integrated systems. It exposes
    codified behaviour and state machines, defines interface boundaries and capabilities, adds nouns and verbs for
    subsystems and collaborations, and adds synonyms and business context for interpreting a prompt.

### Open decisions

- **Skyve Flows and Skyve Forms in admin.** Mike has proposed making both available in the admin module. It is waiting
  on a meeting on the afternoon of 26 November 2026. Until then, where the Skyve Flows editor and inbox live is not
  decided: the model and the engine move into Skyve either way. Skyve Forms (`../skyve-forms`) has not been read for
  this plan.
- **A file process and a runtime process of the same name.** Whether the runtime one overrides, or must be a clone
  under a new name.
- **Statement names.** Whether task steps use new domain-centric names only, or also accept SAIL step names. See
  [Statements](#statements).
- **Where the model lives for the cockpit**: a configured external service, with the provider pluggable. Which
  providers are supported first is a product decision.

### Out of scope

- A language model inside Skyve.
- OWL or RDF authoring.
- Replacing SAIL's browser executors. They remain the way to test what a user sees.
- New process features beyond what this plan needs. Skyve Flows' own backlog (expiry, looping, delegation) stays its
  own.

## Affected modules

| Module | Why |
|---|---|
| `skyve-core` | Engine contracts; task, process and ontology metadata and XSDs; the view guard beside `ActionAvailability`; behaviour `forEach`; process and agent principals; fluent API |
| `skyve-ext` | Task runner, process engine (from Skyve Flows), the two-store repository for processes and behaviours, mining tools, audit additions |
| `skyve-web` | The engine's web delivery (conversation handling beside `SmartClientEditServlet`), the agent service layer, MCP and REST transports, token authentication |
| `skyve-war` | XSD copies, the `agent` UX/UI in the router, kitchen-sink fixtures, H2 tests, admin metadata for the audit additions and for role grants to processes and agents |
| `skyve-maven-plugin` | Validation of task, process and ontology files in `generateDomain`; validation of exported runtime definitions; the mining goal |
| `skyve-flows` (separate repository) | Upgraded to Skyve 10; its model and engine move into Skyve; what remains depends on the open decision above |
| `skyve-forms` (separate repository) | In scope only if the admin proposal is accepted; not yet read. It may be where the generic UI for a task's inputs comes from, which is to be checked |
| docs | A developer guide chapter, `docs/architecture-design.md`, this plan |

## Dependencies

- **Skyve Flows** (`../skyve-flows`): on a Skyve 9 snapshot, last changed in June 2024. It must come up to Skyve 10
  before its model moves.
- **Server-side questions** ([skyve-server-side-confirmation-spec.md](../skyve-server-side-confirmation-spec.md)).
  A question raised by `EXT.getDialogs().ask(...)` becomes a "question pending" result that the agent or the human
  answers. Phase 2 needs it; phases 0 and 1 do not.
- **M1 action availability** ([server-side-action-availability.md](../server-side-action-availability.md)), already
  in. The engine relies on it for actions and applies the same rule to values.
- **SAIL** ([sail.md](../sail.md)). The step vocabulary, the recorder and the browser executors are reused, not
  changed.

## Backward compatibility

Everything is additive and off by default.

- New metadata types are optional. An application with none behaves as today.
- The engine's endpoint and the transports are disabled unless configured, as `/rest` is today.
- The audit additions add columns to `ADM_Audit`. Applications get them through the normal schema update. Existing
  rows have nulls.
- Adding `forEach` to behaviours extends `behaviour.xsd`. Existing action files stay valid. The Skyve Flows behaviour
  editor edits `if` and `set` only; it must still load an action that has other statements, or refuse it clearly.
- A new `agent` UX/UI name must not collide with an application's own UX/UI names. Check the router of assembled
  applications before fixing the name.
- **Skyve Flows and Skyve Forms installations are not a compatibility concern** (Mike, 26 November 2026): there are
  very few. The model, the `FLOW_*` tables and the modules can change freely as they move into Skyve, and no migration
  path is planned. Two behaviours change, which those few sites should be told: an automatic step no longer runs as
  the saving user (decision 11), so a runtime process needs roles granted before its automatic steps work; and an
  invalid approval process holds saves where today it is silently disabled (decision 13).

## Current state observed

### Skyve

- **Conversations.** Each browser tab is a `WebContext` held server side; changes sit in the conversation until Save.
  This gives preview and human approval almost for free.
- **Values through the view.** `ViewJSONManipulator` (skyve-web) walks the view with each widget's visible and enabled
  state, records which bindings are mutable, and applies posted JSON by binding (`applyJSON`). It is tied to
  SmartClient's request shape. PrimeFaces does the equivalent through JSF.
- **Actions through the view.** `ActionAvailability` refuses an action that the view, as applied to the bean, does not
  offer.
- **Access.** `UserAccess` vectors from menus, roles, routes and views stop forced browsing (`EXT.checkAccess`).
- **REST.** `RestService` (`/rest/api/...`) offers document CRUD and queries. It is forbidden by default, and its
  write routes check no document permissions.
- **SAIL.** About 60 step types (navigation, data entry, actions, grids, lookups, captures, assertions). The executors
  drive a browser. There is no loop or branch. The recorder writes SAIL from a user's session.
- **Behaviours.** Metadata actions in a document's `actions` package: `if` (with `then` and `else`), `set`, `invoke`,
  `invokeStatic`, run against one bean with Skyve's expression evaluator. Lightly used: six action files in this
  repository. `bizlet.xml` already carries documentation for the Bizlet class.
- **Run-as precedents.** A job schedule has a required `runAs` user. `SuperUser` is used for system work.
- **Audit.** `admin.Audit` records module, document, bizId, bizKey, operation, timestamp, user and a detail memo. It
  does not record the action or view, and reads are not audited.
- **Sensitivity.** Attributes can declare a `sensitivity` (for example `personal`), which mining can use to redact.
- **Push.** skyve-web has a server-sent events package (`service/sse`), which the cockpit can use to rerender.
- **MCP.** The current specification revision (2026-07-28) is stateless and has no `initialize` handshake. Clients on
  the 2025 revisions still send one. The prototype serves both.

### Skyve Flows

Read on 26 November 2026 at `bb327aa`.

- **Two modules, copied into an application.** `skyveFlows` (the workflow) and `behaviour` (an editor).
- **Definitions are rows.** A `Process` is a template tied to a subject document, with a default role and a tree of
  `ProcessActivity`. Activity types: Container (sequential or parallel, all or any), Acknowledgement, Approval (with
  approve and reject branches), Automatic Action, Behaviour, Call Process, Go To Activity, Notification, Template and
  Wait For Exit. Activities have entry and exit conditions, as a condition name or an expression.
- **Instances are rows.** A `Flow` and its `FlowActivity` rows carry status, owner, response, comments and an
  attachment. There is an inbox.
- **It is driven by saves.** `FlowInterceptor.beforeSave` on a registered subject document starts a flow when the entry
  condition holds, or re-evaluates the open flows. Notifications go after the save.
- **The `behaviour` module is a runtime editor for Skyve's behaviour XML.** It builds `if` and `set` statements,
  validates them with `ExpressionEvaluator` against the subject document, stores the XML on the activity
  (`behaviourXml`) and runs it against the subject bean.
- **Automatic steps run as the saving user**, with document scope widened to the customer, and with no check that the
  user may run the action (`action.execute(bean, null)`). A commented-out `actionUser` asks who they should run as.
- **Startup validation exists and fails open.** `ProcessService.reloadCache` validates every process, follows Call
  Process links, and sets `enabled` to false on any that fail, with a log warning.
- **Open flows are not covered.** A stale condition on an open flow throws inside `beforeSave`.
- **Clone** copies a process and its activities, which is most of versioning.
- **Its own to-do list**: expiry, looping, delegation, non-scalar `set`, and re-triggering ("if I unauthorise a
  timesheet, I've just resubmitted it").

## What the prototype showed

The prototype is not product code. It lives in `~/skyve-sail-work/agent-mcp` (`README.md`, `LANGUAGE.md`,
`EVALUATION.md`, `DECISIONS.md`, `workflows/`). It wraps the SAIL SmartClient executor in a long-lived JVM with an MCP
endpoint, and interprets task files made of SAIL steps plus `forEach` and the behaviour-style `if`. It needed no
change to the SAIL harness. It ran against a restore of Apps with its financial data anonymised. Its files call a task
a workflow.

An evaluation agent that had only the tools (no source, no existing SAIL scripts) did this:

| Job | Outcome | Tool calls |
|---|---|---|
| Fill in a week's timesheet step by step, from a text note | Done and checked on screen | 21 |
| The same through a `FillTimesheet` task | Done first time, from the input schema alone | 3 |
| Write an invoicing task from exploring the screens | Done: validated, dry-run, one real run | 24 |
| Six probes of the language's limits | 3 expressible with workarounds, 3 not | 12 |

An `UploadExpenseClaim` task written afterwards also ran first time, with the receipt attached and the server's GST
split read back.

What worked:

- **Tasks pay off**: 3 calls against 21 for the same result.
- **An agent can write a task from the views alone**, which supports the mining idea.
- **The behaviour `if` and a `forEach` over inputs**, with Skyve's expression evaluator, were enough for these jobs.

What did not, and what this plan does about it:

| Gap found | Addressed by |
|---|---|
| `if` cannot see the record: "open the timesheet if it exists, else create it" needed a page-text scrape | The engine evaluates conditions against the conversation's bean |
| Captured values are display text: a numeric compare broke on `19,075.81` | Typed `get` |
| Type coercion on entry: dates, decimals and times typed as display text | Typed `set` |
| No loop over rows on screen | `forEach` over a collection on the bean |
| Lookups picked the first match: a three-letter search chose the wrong customer | `find` returns candidates; `set` takes an identity |
| No clean-up when a step fails; no way to ask the caller; no task calling another | `onFailure`, `ask`, `call` |
| A file input has nowhere to go: the upload step takes a file on the server's classpath | Content inputs in the service layer |
| The observation lacked lookup options, and at first group titles and grid check box values | The observation contract below |
| Failures took up to 120 seconds to surface | No browser waits in the engine |
| Views reopen on the last-used tab; a zoom-only grid needs a select first | Renderer details that the engine does not have |

## Design

### Layers of the agent service

1. **Transports.** MCP over HTTP, and REST with an OpenAPI description. Both are thin.
2. **Tool registry.** A small fixed set of stratum 1 tools, the task catalogue (search, describe, run) and the flow
   inbox. With tasks saved and shared in the database there can be many, so an agent looks a task up and calls it: two
   calls. A site may also surface chosen tasks as tools of their own. Filtered by customer and role.
3. **Policy gate.** Effect class, confirmation policy, rate limits, and the audit of who acted for whom.
4. **Agent service layer.** Discovery, query, the conversation engine and the task runner.
5. **Skyve as it is.** Repository, persistence, conversations, access control, and the process engine.

### Identity

Every execution has a principal, and the audit records three things: whose permissions it used, who or what triggered
it, and through what it ran.

| Case | Runs as | Guard |
|---|---|---|
| A person acts | Themselves | The view |
| An agent acts for a person | That person, or narrower | The view |
| An agent is the actor of an activity in its own right | Roles granted to that agent | The view |
| An automatic step of a runtime-defined process | Roles granted to that process | Document and action permissions on the bean |
| An automatic step of a file-defined process | Trusted, like Bizlet code | None beyond validation on save |

The rule: the view guards whenever something stands in for a person, and plain permission checks apply to
automation, because nobody is looking at a view.

Roles are granted to a process or an agent by someone who may assign roles to users. A process or agent starts with
none.

### The view guard

This is the rule that lets typed operations run off the views.

- **`set`**: find the input widget bound to the binding, in the view as applied to the current bean, for the `agent`
  UX/UI and the principal. Refuse if there is none, or it is invisible or disabled. Otherwise assign the typed value,
  then fire the widget's declared handlers as if the user changed the field and left it: change handlers first, then
  blur handlers. A rerender runs `preRerender` with the binding as its source.
- **`get`**: allowed only for a binding the view shows. An agent cannot read a hidden field.
- **`action`**: `ActionAvailability`, unchanged.
- **Associations**: the bean set must be one the widget's lookup query (with its filter parameters) would offer.
- **Open and navigate**: `EXT.checkAccess`, unchanged.

What the guard cannot reproduce is behaviour that exists only in a client: custom JavaScript, and client-side event
actions that change a widget's state without a server call. Phase 0 must list these for the views it tries.

### Stratum 1 tools

| Group | Operations |
|---|---|
| Discover | describe the system (a short map), describe a document, search metadata |
| Read | query a list (a metadata query or a document's default, with filter, sort and page) |
| Conversation | open (new, or edit by id), observe, set, get, action, zoom in and out, grid row add and remove, find (lookup candidates), save, ok, cancel, delete, answer (a pending question) |
| Flows | my inbox; an activity is then opened and actioned as a conversation like any other view |
| Other | upload and download content, job status |

Every conversation operation returns the observation: title; fields with binding, type, typed value, required and
disabled; lookup and domain options, or how to search them; grids with typed rows, capped; group titles; the available
actions, implicit ones included; messages and errors; any pending question; and, when zoomed in, which row of which
grid is being edited. A compact form leaves out unchanged parts.

### Tasks

A task is a contract with a pluggable implementation.

**Short-lived.** A task finishes within one exchange. It is not promised to be atomic: it may save more than once, and
a failure part-way leaves what was already saved (see `onFailure`).

**Two stores.** A task lives in a metadata file, written by a developer and validated by `generateDomain`, or in the
database, saved by a user and shareable with others. A database task is a runtime definition and is validated like
one (see [Validation of runtime definitions](#validation-of-runtime-definitions)). Sharing is safe because a task has
no rights of its own: whoever runs it, it runs as them, through the view guard.

**Who runs it.** An agent, through the task catalogue. A person, through a generic UI generated from the contract,
which asks for every input up front. The system, as the implementation of an activity in a process.

**The contract** declares: a description and examples written for an agent; typed inputs and outputs, bound to document
attributes so that the schema, enums and domain values are generated; collections of inputs; content (file) inputs;
roles; an effect class (`reads`, `creates`, `updates`, `deletes`, `external`); and a confirmation policy (`none`,
`preview`, `human`). The same contract can sit on an existing action, Java or behaviour, which makes a single-document
job a task with no steps.

**Preview** uses the conversation: the runner returns the pending changes and the caller commits or discards. A task
that saves part-way cannot preview past that point, and must say so.

<a id="statements"></a>
**Statements** (proposed; settle the names in the phase 3 plan):

| Statement | Meaning | From |
|---|---|---|
| `open`, `new`, `zoomIn`, `zoomOut`, `save`, `ok`, `cancel`, `delete` | Conversation navigation and lifecycle | SAIL steps |
| `set binding expression` | Typed assignment | Behaviour `set` |
| `get binding variable` | Typed read | SAIL capture |
| `action name` | A view action | SAIL `action`; a process's Automatic Action |
| `find binding search variable` | Lookup candidates; fails or asks when not exactly one | New |
| `if condition` with `then`, `else` | Evaluated against the bean, inputs and variables | Behaviour `if` |
| `forEach` | Over a collection input, or a collection on the bean | New, shared with behaviours |
| `ask` | Return a question to the caller, continue with the answer | Server-side questions |
| `onFailure` | Clean-up steps | New |
| `call` | Run another task | New; a process's Call Process |

`if`, `set` and `forEach` are the behaviour statements. They mean the same in an action file, in a Behaviour activity
of a process and in a task; only the guard differs with the host (decision 16). Behaviour `invoke` and `invokeStatic`
stay out of task steps: a task reaches Java through an action or a class implementation, where permissions apply.

A task does not wait for a person for longer than one exchange. `ask` returns a question to the caller of this
request. Waiting for an approval is an activity in a process.

The expense claim from the prototype, in this form:

```xml
<task name="UploadExpenseClaim" module="cashbook" effect="creates" confirm="preview">
  <description>Makes and submits an expense claim for the signed-in member of staff. ...</description>
  <inputs>
    <input name="receiptDate" document="ExpenseClaim" attribute="receiptDate" />
    <input name="category" document="ExpenseClaim" attribute="category" />   <!-- an identity, from find -->
    <input name="amountIncTax" document="ExpenseClaim" attribute="amountIncTax" />
    <input name="comment" document="ExpenseClaim" attribute="comment" />
    <input name="receipt" type="content" />
  </inputs>
  <steps>
    <new document="ExpenseClaim" />
    <set binding="receiptDate" expression="{el:receiptDate}" />
    <set binding="category" expression="{el:category}" />
    <set binding="amountIncTax" expression="{el:amountIncTax}" />   <!-- fires the blur rerender: the server splits the GST -->
    <set binding="comment" expression="{el:comment}" />
    <if condition="{el:not empty receipt}">
      <then><set binding="attachment" expression="{el:receipt}" /></then>
    </if>
    <get binding="amountTax" variable="tax" />
    <action name="Submit" />
  </steps>
</task>
```

**Relationship to SAIL.** A SAIL `dataEnter` is a `set` whose display text is converted by the attribute's converter.
A browserless SAIL executor can therefore be built on the engine, which gives fast functional tests, and a recorded
SAIL script can be converted to a task draft mechanically.

**Validation.** `generateDomain` validates task files like other metadata: documents, bindings, actions, input types,
and references between tasks.

### Processes

The Skyve Flows model becomes Skyve metadata: a process, its tree of activities, and their conditions, with the same
activity types. What this plan adds or changes:

- **Two stores** (decision 9). A process loads from a metadata file or from the database. The runtime editor reads and
  writes the same model. The same applies to the behaviour inside a Behaviour activity.
- **An agent as actor** (decision 10). An activity can be assigned to an agent as it is to a person. The agent sees it
  in its inbox and actions it through the view.
- **An activity implemented by a task** (direction 31). The activity names a task. An agent actor runs it. A human
  actor is shown the task's generic UI, which asks for all the inputs up front, and the task does the rest.
- **Identity of automatic steps** (decision 11), as in [Identity](#identity). Today these run as the saving user with
  no action permission check.
- **Behaviour statements shared with tasks.** A Behaviour activity gains whatever the shared statements gain.

Automatic steps keep running inside the triggering save, as now. A step that calls an outside service should not: it
belongs in a job or an agent-run activity, so that a slow or failing service does not fail the user's save.

<a id="validation-of-runtime-definitions"></a>
### Validation of runtime definitions

A runtime definition (a process, a behaviour or a task saved in the database) names documents, bindings, conditions,
actions and roles, and a deployment can remove any of them. One validator runs at four points (decision 12):

1. **When edited**, as Skyve Flows does now.
2. **In the build, before deploying.** A site's runtime definitions are exported as XML and validated against the new
   metadata. This is what one model in two stores buys: the same validator works on both.
3. **At startup.** The server always starts. An invalid definition is marked invalid with its errors, and the flow
   administrators are told.
4. **When executed.** An open flow that hits an error is parked with the reason. It does not throw into the user's
   save.

**When a process is invalid** (decision 13), it either skips (no flow starts, as today) or holds (a save that would
start or advance it is refused with a clear message). Each process says which. The default is to hold when the process
contains an approval or an acknowledgement, and to skip otherwise: a missed notification is a nuisance, a missed
approval is a control failure.

**Versions.** Once a flow has started from a definition, an edit makes a new version, and open flows finish on theirs.

Validation is against each customer's metadata, since definitions and overrides are both per customer.

### Ontology

An overlay on documents. Generated from metadata: documents, relations, enums, constraints, descriptions, and
lifecycles from process definitions. Hand-authored only for what metadata cannot say:

- **Business meaning**: synonyms ("job" means `Project`), units, what a term means in this business.
- **Lifecycles** that no process describes: status transitions and what triggers them.
- **Rules that live in Java**, recorded in `bizlet.xml` documentation, which already exists for this.
- **Cross-system identity**: this record is that one in another system, reachable through that API or MCP server.
- **Boundaries and capabilities**: where one subsystem ends and another begins, and the nouns and verbs each offers
  to the others.

Mike's note places the ontology mainly with integrated systems (direction 32). Synonyms and state machines can also
help an agent on a single system; the phase 5 measurement decides how far to take it there.

It is served as a short map plus detail on demand, never loaded whole. Three cautions shape it: context is not free; a
stale description is worse than none, so generate and validate it; and a description is not enforcement, so the server
still enforces every rule.

### Mining

Inputs: metadata, a database copy, audits, and recorded SAIL where it exists. Output: draft task, process and ontology
XML for a person to review.

- **Minable**: lifecycles, fields that change together, document sequences such as timesheet then invoice, and who
  approves what.
- **Inferred today**: which action caused a change. Decision 25 fixes this for new data.
- **Evaluation**: replay a drafted task on the database copy and compare with what the audits say happened.
- **Privacy**: redact by `sensitivity` before a model sees a copy. The prototype's rig has a repeatable step that
  anonymises financial data in a restore; it is a model for an application-level tool.
- **Caution**: audits show what people did, workarounds included, not what is allowed.

**On the fly** (direction 30). After an ad hoc request, the user can save what was just done as a task. The engine has
the typed operations of the session; the values that would vary become inputs; the user reviews the result, saves it to
the database and can share it or attach it to an activity of a process. A developer can later promote it to a metadata
file. The prototype did this by hand: `UploadExpenseClaim` was written from the log of a claim made step by step.

### Cockpit

The cockpit's components (direction 26) are AI chat, in-app contextual messaging for teams, notifications and
reminders, and the flow inbox. This plan designs the chat and the inbox. Messaging, notifications and reminders are
listed, not designed here.

- **Chat on the user's own conversation.** The agent fills the form the user is looking at, and the user presses Save:
  an ad hoc request ends in a filled-out but unsaved view (direction 27). The server pushes a rerender to the client
  over SSE.
- **Guidance.** The same channel lets an agent walk a user through the system instead of doing the work for them.
- **Save as a task**, as in Mining above.
- **Inbox.** Activities waiting for the user, and for agents they are responsible for.

Skyve is the MCP server; the model is an external service.

### Security

- An agent acting for a user has that user's permissions or fewer, never more.
- A runtime-defined process, or an agent in its own right, has only the roles granted to it.
- Record content is untrusted input to an agent. Deletes and external effects need human confirmation whatever the
  agent decides.
- Tokens are scoped, short-lived and revocable. Every write records who acted for whom.
- The transports are off by default and bound to configuration, as `/rest` is.

## Implementation plan

Two tracks. The agent track is phases 0 to 3. The process track (P1 to P3) can start once Skyve Flows is on Skyve 10,
and meets the agent track at P3.

| Phase | Deliverable | Size |
|---|---|---|
| 0 | Spike: typed, view-guarded operations on one conversation, no browser | Small; carries the risk |
| 1 | Engine contracts, service layer, read-only tools, MCP transport, token authentication for an agent acting for a user | Medium |
| 2 | Full operation coverage: grids, zoom, lookups, content, questions; the `agent` UX/UI; browserless SAIL executor | Large |
| 3 | Task metadata: XSD, loading from files and the database, validation, runner, preview and commit, the task catalogue, saving a session as a task; behaviour `forEach` | Medium to large |
| P1 | Skyve Flows on Skyve 10; the process and behaviour model as Skyve metadata with two stores | Medium to large |
| P2 | Process identity and role grants; the four-point validator; hold or skip; parking; versions | Medium |
| P3 | An agent as the actor of an activity; the inbox as a tool; an activity implemented by a task; the generic UI that lets a person run a task (needs phases 1 and 3) | Medium to large |
| 4 | Ontology: generated baseline, overlay XML, exposure as resources | Medium |
| 5 | Mining tool, audit additions, replay evaluation | Medium |
| 6 | Cockpit: chat on the user's conversation, guidance, inbox. Messaging, notifications and reminders need their own plan | Medium, plus the unplanned components |

### Phase 0: the spike

**Goal.** Show that `open`, `observe`, `set`, `get`, `action`, `save` and `delete` can run against a conversation with
no browser, with the view guard, and give the same result as the browser.

**Where.** A gated endpoint in skyve-web, off by default behind a setting and a role, as the SAIL recorder is. Reuse
`ViewJSONManipulator`'s view walk and apply path where it can be driven without SmartClient's request shape; note
where it cannot.

**Views.** A kitchen-sink fixture with a field whose blur handler rerenders, a conditionally disabled field, a
conditionally invisible field, a lookup and a conditional action. Then Apps' expense claim on the rig, compared with
the prototype's browser run (22.00 entered; 20.00 and 2.00 computed).

**Questions it must answer.**

1. Can the SmartClient apply path be reused, or must a renderer-neutral core be extracted first?
2. What exactly does a rerender do on the server, and what do client-side event actions do that the engine must
   reproduce or refuse?
3. How does the engine hold and lock a conversation (cache, CSRF token, concurrent requests)?
4. How is an association checked against its lookup query and filter parameters?
5. What does `ActionAvailability` need from "the view as last rendered" when there is no renderer?

**Exit.** The expense claim runs end to end with no browser and matches the browser's result. Each refusal is shown:
a disabled field, an invisible field, a binding not on the view, an action not offered, a record the lookup would not
offer.

### Later phases

Each gets its own plan. Notes for them:

- **Phase 2 parity.** Run the same SAIL script through the browser executor and the engine and compare the saved
  beans. This is the regression test that keeps the two paths from drifting.
- **P1 first step.** Before moving anything, read Skyve Flows against Skyve 10 and list what breaks, and decide the
  open question of where its modules live.
- **P1 freedom.** With no installations to protect, P1 can reshape the Skyve Flows model where the metadata form
  wants it different, and need not keep the runtime tables as they are.
- **Phase 5 measurement.** One fixed set of jobs on one application, run three ways (stratum 1 only, plus tasks, plus
  ontology), comparing success rate, calls and tokens. Decide how far to take the ontology on that evidence.

## Acceptance checklist

Phase 0:

- [ ] The kitchen-sink fixture view is driven end to end through the endpoint by an H2 test, with no browser.
- [ ] A `set` on a field with a blur rerender runs `preRerender` and the observation shows the computed values.
- [ ] Each of the five refusals in the exit criteria has a test that fails without the guard.
- [ ] The Apps expense claim matches the browser run on the rig.
- [ ] The five questions are answered in writing, with the code read cited.
- [ ] A recommendation: proceed to phase 1 as planned, or extract a renderer-neutral core first.

Programme (each phase's plan refines these):

- [ ] An agent with a user's token can do nothing that user cannot do in the UI.
- [ ] Every write records whose permissions were used, who or what triggered it, and through what it ran.
- [ ] Task, process and ontology files that reference a missing document, binding or action fail `generateDomain`.
- [ ] The same SAIL script gives the same saved data through the browser and through the engine.
- [ ] A runtime-defined process with no roles granted cannot run an automatic action or change a record.
- [ ] A site's exported runtime definitions are validated in the build, and a removed binding fails it.
- [ ] An invalid process that contains an approval holds the saves that would start it, with a clear message.
- [ ] An open flow that hits a stale definition is parked with the reason, and the user's save succeeds.
- [ ] An agent completes an activity assigned to it, and the audit shows the agent.
- [ ] A task that one user saved from a session runs for another user with that user's permissions and no more.
- [ ] A person runs a task through the generic UI, giving every input up front, with the same result as an agent.
- [ ] With the feature not configured, an application behaves exactly as before.

## Better engineering notes

- **Two execution paths.** The engine and the renderers can drift. The parity test in phase 2 is the control; treat a
  parity failure as a defect in one of them.
- **Tool and context size.** Keep stratum 1 to a fixed set of tools. Do not generate a tool per document.
- **MCP is moving.** The 2026-07-28 revision changed the handshake. Keep the transport thin so that a revision costs
  little.
- **Skyve Flows' re-triggering problem** ("if I unauthorise a timesheet, I've just resubmitted it") is a process
  design issue that mining and agents will both run into. It is not solved here.
- **Other runtime content names metadata too**: communication templates, report templates, import definitions,
  snapshots. The four-point validator could later cover them.
- **Prototype findings about SAIL**, separate from this plan: step outcomes are not structured; a `testSuccess` after
  a `login` in another call behaves differently; SmartClient `dataGridZoom` on a non-inline grid needs a select first.
- **Mined definitions are drafts.** They need review, and a replay result, before anyone relies on them.

## AI validation plan

- Phase 0: H2 tests through the real servlets for the fixture view, following `docs/ai/test-patterns.md`; the refusal
  tests shown failing without the guard; the Apps run on the rig compared with the prototype's recorded result.
- Build and test in a mirror of the checkout, not in the shared checkout.
- From phase 1: contract tests for the service layer; the transports tested against a real MCP client and with plain
  HTTP.
- From phase 2: the SAIL parity run on the kitchen-sink and admin suites.
- Process track: Skyve Flows' own tests carried across where the behaviour is kept, and rewritten where the model
  changes; new tests for role grants, hold and skip, parking and versions, each shown failing first.

## User validation suggestions

- Try the prototype: `~/skyve-sail-work/agent-mcp/README.md`. Run `FillTimesheet` and `UploadExpenseClaim`, and edit
  a task file to feel the language.
- After phase 0: watch one claim made with no browser, then try to set a disabled field and a hidden one.
- Read the statement table against two real jobs from Apps that the prototype did not try, and see what is missing.
- Take one real Skyve Flows process, such as a timesheet approval, and walk it through the identity table: who does
  each step run as, and what roles would the process need.
