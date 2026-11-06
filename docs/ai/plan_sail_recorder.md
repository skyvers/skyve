# Plan: SAIL recorder

Status: approved to build, 2026-11-04. See [Decisions](#decisions-mike-2026-11-04). On 2026-11-06 Mike said to build it on the `sail` branch. Phase 0 (language gaps 1–4) is done; the recorder itself has not been started. Other open SAIL work is in [plan_sail_completion.md](plan_sail_completion.md).

**This run (Mike, 2026-11-06):**

- Build phases 1–5 in a `sail-recorder` stream worktree, recreated from `sail`. The worktree, ports, build, compatibility and decision rules are in [How the work is done](plan_sail_completion.md#how-the-work-is-done).
- Merge the recorder into `sail` before the Apps A/B rerun, so the rerun covers it. The gate keeps it off by default, but it changes shared pages (`home.xhtml`, the PF theme heads, the Control Panel), the admin module (the new role) and the JSON config.
- Ask Mike about the phase 3 overlay UX before building it. Decide the other recorder internals yourself (which SC methods to wrap, the endpoint's shape, the Control Panel layout), and log them in `DECISIONS.md`.
- **Round trip, the test for every phase:** run an existing SAIL script through the Selenide executor with recording on, download the recording, then replay it through the same renderer's executor, unedited, and expect it to pass. Make it an IT in the kitchen sink suite, and use admin workflows as well. For phase 3, have the IT add assertions through the overlay. For phase 4, run the same scripts in PF.

## Summary

A recorder that watches a person use a Skyve app, in SmartClient (SC) or PrimeFaces (PF), and writes a SAIL script of what they did. The script is a first draft. A person or an AI then edits it into a test: adds assertions, turns values into parameters, and removes steps that aren't needed.

The recorder runs in the browser and is built into Skyve's client code, not shipped as a browser extension. The server decides on each page whether to send it. Users who aren't recording download no recorder code, and Skyve's normal client code has no recorder checks in it.

The recorder builds on the language in the `sail` branch: `rowContaining`, `navigateMenu`, `switchMode`, the `{run}` token and the phase 0 steps (list filters and sorting, `match`, lookup picks by text, `captureValue` with `{var:name}`). It is built on that branch, not on master. The old `sail-recorder` branch (worktree `~/skyve-sail-recorder`, at afaf79768b) has no commits of its own and predates phase 0, so don't use it.

## Is it worth building?

AI can already write SAIL. The admin suite on the `sail` branch was written by agents, and `org.skyve.impl.generate.sail.Generator` produces SAIL from menus and views. Writing a script to cover views mechanically is solved.

What neither can know is which business workflows matter. That means the order of steps, realistic values, which records to use, and what a correct result looks like. That knowledge sits with the people who use and test the app. Most of them won't write SAIL XML, and they may not be able to describe a workflow precisely enough in English either.

The recorder is worth it for these cases:

- **Domain experts capturing workflows** without learning SAIL.
- **Reproducing bugs.** "Record what you did" gives a starting script, not a vague description.
- **Giving AI a starting point.** A recording with its context is much easier for an AI to turn into a good test than a blank page.

It isn't worth it if the only people writing SAIL are Skyve developers working with AI. In that case a cheaper route works: describe the scenario in English, let the AI write SAIL, run it, and fix it.

A check was proposed before building: have AI write SAIL from English descriptions of real workflows and see where it goes wrong. Mike chose to build without it (decision 4).

The Biz Hub Apps regression run (2026-11-05) did that check by accident. Agents wrote 9 Apps workflows from the app's metadata and short English answers from Mike, and all ran green against a prod restore in both renderers. The mechanics were not the hard part. What the agents could not work out alone was who does what (personas and their roles), which records to use, and what correct looks like. That supports the case above: the recorder's value is capturing what people actually do.

## Goals and non-goals

Goals:

- Record SC and PF sessions as SAIL steps that name bindings and actions, never DOM ids or screen coordinates.
- No cost to real systems: no extra bytes downloaded, no extra code run, unless recording is allowed for that user and turned on.
- Output that is easy to edit: readable, stable, and annotated with what was on screen.

Non-goals:

- Replaying production traffic unchanged. Recordings point at data that won't exist in a test database. Fixing that means capturing data, which is a separate project.
- A browser extension.
- Generating assertions automatically from server responses. They would be noise. Assertions are added on purpose, in the recorder (phase 3) or by whoever edits the script.

## Why record in the browser, not on the server

- SC is a rich client, so the server only sees data operations. It never sees tab selection, most grid row selection, Cancel/zoom-out handled in the client, or menu navigation, though some of these can be guessed from the fetch that follows.
- PF is better, because each ajax postback names the component that sent it. But PF tab changes stay in the browser unless they're wired to ajax.
- Field values reach the server in batches at the next request. The entry order is lost.
- Assertions need someone to say "check this", and that only happens in the browser.

The server still has a role: it checks the recorded steps against the view metadata and writes the XML (see [Server side](#server-side)).

## Zero cost to real systems

### Gate

Recording needs all three of these:

1. **Environment flag.** A new `sail.recording` setting in the JSON config. It defaults to `false`. If it is on when `environment.identifier` is null (production), Skyve still allows recording but logs a warning at startup. The Control Panel also shows a warning when recording starts (decision 2).
2. **Role.** A new `admin.SAILRecorder` role, separate from `DevOps`, so testers can record without getting DevOps powers (decision 1).
3. **Recording turned on for this session.** A Start/Stop control in the Control Panel's SAIL section sets a session attribute.

### Loading

If any part of the gate fails, the server sends nothing extra. There's no stub and no hidden button, so the page is byte-for-byte what it is today.

If the gate passes, the server adds one `<script>` tag after the normal client bundle:

| Renderer | Where the tag goes | Notes |
|---|---|---|
| SC | `skyve-war/src/main/webapp/desktop/home.xhtml`, after `skyve-min.js` | SC is a single page. Starting a recording mid-session can also insert the script on the fly, without a reload. |
| PF | each theme's `head-resources.xhtml` (editorial, ultima, ecuador, diamond, iframe), after `skyve/prime/skyve-min.js` | PF does a full page load per view. The tag must be sent on every page while recording is on, and recording state lives on the server, not in the page. |

The tag is wrapped in `<c:if>` / `rendered=` against a bean property, for example `#{desktop.sailRecording}` or `#{bean.sailRecording}`. That costs one boolean check per page render on the server.

The recorder ships as separate bundles: `skyve/sail/recorder-sc-min.js` and `skyve/sail/recorder-pf-min.js`. They are never merged into `skyve-min.js`.

**Rejected: a small loader stub in every page that fetches the recorder when you press a key.** The stub would be in everyone's bundle, and it gives nothing that gating on the server doesn't.

### No hooks in core code

Skyve's own client code gets no `if (recording)` checks. The recorder attaches itself once it loads:

- **DOM listeners on `document` in the capture phase** for clicks, changes and key events, mapped back to widgets.
- **SC:** it wraps a few of Skyve's own SC class methods when it loads, for example the action-button handler, the form item-changed handler and the grid select handler. It calls the original method and then records. Which methods to wrap is decided in the phase 1 prototype.
- **PF:** it listens to PrimeFaces and Faces ajax events to see which component sent each request and when the response is done. It reads `PrimeFaces.widgets` to identify widgets.

Because the recorder only ever runs for a recording session, its own speed matters little. It still must not slow down the page it's recording.

## Mapping gestures to SAIL steps

The recorder's main job is the reverse of what the executors do: they turn a binding into a locator, and the recorder turns a widget back into a binding.

- **SC:** form items are already named by their binding, and grids and buttons know their data source and action. Most steps can be read straight from the widget.
- **PF:** component ids are generated. The page also gets a client-id → binding map for the current view, built with the same mock render that the SAIL generate-edit-context uses (`skyve-web/.../sail/execution`). The map is only built and sent while recording.

| Gesture | SAIL step | Notes |
|---|---|---|
| Menu click | `navigateMenu` / `navigateList` / `navigateEdit` | Prefer `navigateMenu`, because it's what the user did. |
| Type into or change a field | `dataEnter` | Keep only the final value before the next action or server round trip. For combos and radios, record the option's code with `match="code"` (language gap 2). Write a newline in a text area or rich text value as `&#10;` (marshalling keeps it). |
| Filter or sort a list | `listGridFilter` / `listGridClearFilter` / `listGridSort` | Built (gap 1). Always write `operator`, except for `contains`: enum, boolean, date and time columns need `equals` or another operator the column takes. Which operators a column takes differs by renderer and column type; see [Filtering lists](../sail.md#filtering-lists). Write enum values as the option's label, as the filter shows it. |
| Select a tab | `tabSelect` | Client-only in both renderers. This is the case server capture can't handle. |
| Button or toolbar action | `action`, `ok`, `save`, `delete`, `cancel`, `zoomOut`, `remove` | Use the built-in step when one exists. |
| Grid row zoom or select | `listGridZoom` / `dataGridZoom` / `…Select` with `rowContaining` | Use the row's bizKey, or its most distinctive cell, never the row number. Fall back to `row` only if no cell is unique. |
| Grid New | `listGridNew` / `dataGridNew` | |
| Lookup pick or autocomplete | `lookupDescriptionPick` / `…AutoComplete` | Use `rowContaining` or `pick` with the chosen description, never a row number (language gap 3). Always emit the lookup steps, never `dataEnter`, because PF rejects `dataEnter` on a lookup. |
| Upload | `upload` | Record the file name. The editor supplies the file. |
| Confirm dialog | `confirm="false"` when the user declined | |
| Login / logout | `login` / `logout` | The password is never recorded. Write a placeholder for the editor to fill in. |
| Anything the recorder can't map | `<!-- unmapped: click on #id "label" -->` comment | Never drop a gesture silently. The editor needs to see the gap. |

## Context for whoever edits the script

A bare step tells an editor too little. The recorder writes context next to each step as XML comments in the script itself. There is no separate file (decision 3).

- for a row pick: the bizKey and the visible values of the chosen row;
- for a data entry: the field label and the value shown before the change;
- after each action: messages and validation errors shown;
- after each server round trip: the bound values that changed on screen. Only the changes are listed, to keep the script readable;
- one heading comment per screen: module, document, view and renderer.

With this, an AI can add `testValue`/`testMessage` steps, put `{run}` into values that must be unique, and find steps that don't matter.

The output stays readable: one step per line, attributes in a fixed order, and steps grouped at server round trips. Diffs stay small when someone edits it.

## Server side

- A small endpoint receives batches of recorded steps as JSON for the current session. It is session-scoped and checked against the same gate as above. It uses Skyve's normal CSRF protection.
- It checks each step against the metadata: the binding exists in the view, the action exists, and the row text was visible. It marks steps it can't confirm as comments; it doesn't drop them.
- On Stop, it builds an `Automation`. The Control Panel offers it for download as XML, next to the existing Download Client SAIL.
- Recorded values are user data. The recording is kept only in the session until downloaded, never written to the database, and is gone when the session ends.

## SAIL language gaps

Status: gaps 1–4 were built on `sail` on 2026-11-05 (core 87e1c11cf9; PF fae0246ae9 and SC 62ef938e79; filters on every column type ddd405ab19 and b811c3175d) and are documented in [docs/sail.md](../sail.md). The proposals below are kept for the reasoning; where the built language differs, `docs/sail.md` is right. Gaps 5–10 are still proposals.

These were found by comparing the language on `sail` (afaf79768b) with what a user can do in SC and PF. Gaps 1–4 come up in nearly every real recording, so they come before recorder phase 1. Gaps 5–10 wait for the recorder phase that needs them.

The phase 2 principle still holds: extend existing steps with optional attributes where the meaning fits, add a new op-code only for a new gesture, and keep existing scripts unmarshalling unchanged. The XSD is regenerated from the JAXB annotations with `XMLMetaData.main`, never edited by hand. Each new step gets an `Executor` method, a no-op in the `Generator`, and both interpreted executors.

### Before phase 1

**1. Filtering and sorting lists.** No step filters, searches or sorts a list. `rowContaining` only searches rows that are loaded: the SC grid loads as you scroll, and the PF Vue grid pages. Without a filter, a recorded "find X and open it" stops working once the list is longer than one screen.

```xml
<listGridFilter query="qContacts" column="name" value="Smith{run}" operator="contains"/>
<listGridFilter query="qContacts" column="name"/>   <!-- no value: clears that column -->
<listGridClearFilter query="qContacts"/>
<listGridSort query="qContacts" column="name" descending="true"/>
```

- The list is named the same way as the other list grid steps: `module`/`document`/`query`/`model`.
- `column` is the column's binding.
- Repeated filters on different columns are ANDed, which matches the SC filter row and the PF Vue column filters.
- `operator` is one of `contains` (the default), `startsWith`, `equals`, `notEquals`, `lessThan` or `greaterThan`. That is the set both renderers support.
- The SC advanced filter can build nested AND/OR criteria. When what the user built can't be written as per-column ANDs, the recorder emits it as an unmapped-gesture comment.
- `listGridSort` is needed because `rowContaining` picks the first match in displayed order.

**2. Combos and radios by code.** In both renderers, `dataEnter` on a combo or radio takes a zero-based option index. A recorded `value="2"` can't be read and breaks when the options change.

```xml
<dataEnter binding="status" value="Active" match="code"/>
<dataEnter binding="status" value="Active Member" match="label"/>
```

- `match` is one of `index` (the default, so existing scripts keep working), `code` or `label`.
- The recorder emits `code`, because labels change with the language.
- `testDataEnter` and the `Generator` can switch to `code` as well.

**3. Picking lookup rows by content.**

- `lookupDescriptionPick` gets `rowContaining`, with the same rule as `GridRowStep`: exactly one of `row` and `rowContaining`.
- `lookupDescriptionAutoComplete` gets an optional `pick="text"`, which chooses the first suggestion containing that text instead of the first suggestion.

**4. Reusing values the app generated.** A recording that creates a record and later finds it again by its generated number (an invoice number, a bizKey) can't replay. `{run}` only covers values the user typed.

```xml
<captureValue binding="invoiceNumber" variable="invoice"/>
...
<listGridFilter query="qInvoices" column="invoiceNumber" value="{var:invoice}" operator="equals"/>
```

- `{var:name}` is replaced wherever `{run}` is replaced today.
- Capturing an undefined variable, or using one, fails the step.
- The recorder emits a `captureValue` when a value the user types later, or a `rowContaining`, equals a value the app showed earlier that the user didn't type.

### Later, when the recorder phase needs them

5. **`listGridEdit`**: inline editing of a row in an SC list grid (the list grid's Edit item). It mirrors `dataGridEdit` and is SC only.
6. **List grid tools:** `<listGridTool query="…" tool="refresh|clearSelection|export|chart"/>` for toolbar features that aren't view actions.
7. **Tags and snapshots:** select a snapshot, create, update or delete one, and tag or untag rows. These need a way to answer a text prompt, because tag and snapshot names are typed into a prompt and `confirm` only answers yes/no. The recorder must at least note when a snapshot was active, because it changes the list's filter.
8. **`dataGridMove`:** reorder a row of an ordered data grid, with `row`/`rowContaining` and a target position.
9. **Trees, maps and calendars** beyond navigation: expanding tree nodes, zooming into tree rows, and clicking map features and calendar events.
10. **Browser and output:** `navigateBack`, `refresh`, links or reports that open a new tab, and checking a download's contents.

### Found by the Apps run (2026-11-05)

These came from writing real workflows. Details and suggested shapes are in [plan_sail_completion.md](plan_sail_completion.md), section 1.

- **`captureValue` reads only a view widget** (completion 1.2). The rule above, "emit a `captureValue` when a value the user types later equals a value the app showed earlier", needs `captureValue` to read a list cell or the view title too, because generated numbers often appear only there.
- **Links in grid rows** (completion 1.3). Until there is a step, the recorder writes an unmapped-gesture comment for them.
- **Workflows across renderers** (completion 1.4). A recording session is one user in one renderer, so a workflow that hands over between a client and staff is several recordings. The runner that shares `{run}` and variables between scripts would let them replay as one.
- **The signed-in user's roles.** A script that signs in as a less privileged user only replays correctly if the IT registers that user's roles with `viewRoles` (see [The test user](../sail.md#the-test-user)). The recorder knows the roles of the user being recorded, so it should write them into the script header comment, ready to copy into `viewRoles`. Groups have to be expanded to roles (completion 1.5).
- **Role-based routers.** An app's UX/UI selector may route by role (Apps sends client users to a support centre UX/UI). The recording's `uxui` is the one the user actually got.

### Not language gaps, but they affect recording

- Some steps throw in PF: `lookupDescriptionNew/Edit`, `dataGridSelect/Edit`, `navigateTree` and `navigateCalendar`. A recording is tied to the renderer it was made in through the automation's `uxui`.
- PF `dataEnter` rejects component types it doesn't know, such as geometry and comparison. Recording those emits an unmapped-gesture comment until the executors support them.
- `upload` only supplies the file. Cropping or drawing on an image in the upload screen can't be expressed.

## Phases

0. **SAIL language gaps 1–4** (see [SAIL language gaps](#sail-language-gaps)), in the core and both executors, with conformance scripts in the kitchen sink. **Done** on `sail`, 2026-11-05.
1. **SC core.** The gate, loading, `navigateMenu`/`navigateEdit`, `dataEnter`, `tabSelect`, actions, `listGridZoom` with `rowContaining`, unmapped-gesture comments, and download. Done when a recording of an admin workflow replays green through the existing SC Selenide executor without edits.
2. **Context.** Comments and server-side checks.
3. **Assertions while recording.** A recorder overlay: alt-click a field to add `testValue` with its current value, or alt-click a message to add `testMessage`.
4. **PF.** The client-id → binding map and the same step set. Done when the same workflow recorded in PF replays green through the PF executor.
5. **The rest.** Data grids, lookups, upload, calendar/map/tree navigation, and confirm dialogs.

## Decisions (Mike, 2026-11-04)

1. **Role:** add a new `admin.SAILRecorder` role. Don't reuse `DevOps`.
2. **Production:** recording is allowed when `environment.identifier` is null, with a warning. It is not refused.
3. **Context:** screen context goes in XML comments in the script. No separate file.
4. **No phase 0 check:** build without first testing AI-written SAIL.

## Risks

- **Replay timing.** A recorded script is subject to the same SC timing issues as a hand-written one. The recorder doesn't fix flakiness. The executors' condition waits still have to.
- **Widget changes.** The SC method wrapping depends on Skyve's SC class internals. A comment at each wrapped method in `skyve-web/src/js/desktop` should point at the recorder, so changes there keep it in mind.
- **Data dependence.** `rowContaining` and lookup search text still assume the data exists. Recordings work best against the same seeded test data the suite uses.
- **Personal data.** A recording made against real data holds real names, emails and amounts in its values and context comments. The Apps scripts kept these out by passing them in at run time and filtering on structure (status, dates, `{run}`). Whoever edits a recording must do the same before it is committed.
