# Plan: completing SAIL

Status: open work list, 2026-11-06. It collects every SAIL suggestion from the
Biz Hub Apps regression run (2026-11-05) and the earlier SAIL completion run
that is not done yet. The recorder has its own plan:
[plan_sail_recorder.md](plan_sail_recorder.md).

Work happens on the `sail` branch (worktree `~/skyve-sail`). Skyve 10 is
released from `sail`. User-facing limits are listed in
[Known Limitations](../sail.md#known-limitations), and Skyve runtime bugs in
[SAIL runtime issues](../sail-runtime-issues.md). This file says what to do
about them.

## How the work is done

This section applies to this plan and to the recorder plan.

### Where

- All paths are relative to `~/skyve-sail`. Never read or edit anything in the
  shared checkout `~/_/skyve`, and never switch branches there. Its plan
  copies are stale.
- The orchestrator works on `sail` and merges into it. Agents work in stream
  worktrees off `sail`, one branch each, for example `sail-pf`, `sail-sc`,
  `sail-lang` and `sail-recorder`. Reset an old stream branch to `sail` before
  reusing it. Commit on `sail` and the stream branches only, and never push.
- The run-skyve skill is not tracked. Copy `.claude/skills/run-skyve` into
  every new worktree.

### Servers, builds and the Apps rig

- **Disposable servers only.** Always pass `-Dsail.baseUrl`. Run SAIL ITs one
  at a time per server (`-DforkCount=1`, named tests, the `sail-it` profile).
- Each worktree gets its own run-skyve `SKYVE_RUN_DIR` (under the session
  scratchpad) and a fixed `SKYVE_PORT_OFFSET`:

  | Worktree | Offset | HTTP |
  |---|---|---|
  | `sail` | 300 | 8380 |
  | `sail-pf` | 310 | 8390 |
  | `sail-sc` | 320 | 8400 |
  | `sail-lang` | 350 | 8430 |
  | `sail-recorder` | 360 | 8440 |

- **Hands off:** port 8080 (Mike's server), the Apps A/B rig (offsets 330 and
  340, so HTTP 8410 and 8420) and MySQL 3358. Don't stop, remove or prune any
  Docker container or volume (the rig's `apps-ab-mysql57-lc` must survive for
  the Apps rerun).
- Build in an rsync'd mirror, not in the worktree (the IDE's compiler
  overwrites `target/classes`). Use `mvn -o`. Never `mvn install` into
  `~/.m2`: master and `sail` are both `10.0.0-SNAPSHOT`, so it would replace
  the master snapshot Mike builds against. If an install is needed, use
  `-Dmaven.repo.local=<scratchpad>/m2`.
- No Spotless. Format by hand.

### Language and harness

- **Language changes** follow the phase 2 principle: extend an existing step
  with an optional attribute where the meaning fits, add an op-code only for a
  new gesture, and keep existing scripts unmarshalling unchanged. Regenerate
  the XSD with `XMLMetaData.main` (never edit it by hand; it has a copy in
  `skyve-core/src/test/resources/schemas/sail.xsd`). Each new step gets an
  `Executor` method, a no-op in the `Generator`, both interpreted executors and
  a kitchen sink conformance script.
- **No fixed sleeps or pauses** in the harness. Wait on a condition only when
  it is needed, and say in a comment which event it waits for.
- Update `docs/sail.md` (op-code tables, PF and SC columns, Known Limitations)
  in the same change.

### Backward compatibility

The Apps A/B rerun uses the Apps SAIL scripts and ITs unchanged, so keep all
of these working:

- the SAIL language: existing scripts unmarshal and run the same way;
- the harness Java API the Apps ITs call: `BrowserConfiguration`, `Devices`,
  `AbstractPrimeFacesInterpretedSail`, `AbstractSmartClientInterpretedSail`,
  `AbstractH2Test`, the `*Selenide` and `*InterpretedWebDriverExecutor`
  classes, `SailViewRoles`, `SailUsers` and `SailServerLock`;
- the `-Dsail.*` system properties.

**Check before finishing:** in a mirror, copy
`~/bizhub-apps-sail/Apps/src/test/java/sail/apps/` (read only; it is
uncommitted work from the Apps run) into `skyve-war/src/test/java/sail/apps/`
and run `test-compile`. Those ITs only import Skyve core/web classes, the
`util` harness and JUnit, so they compile without Apps. Don't start them.

### Definition of done

- The kitchen sink and admin suites green 3 times headless and 3 times headed
  (`-Dsail.headless=false`), in both renderers.
- The full reactor unit tests green.
- No new Eclipse null warnings (see the batch ECJ recipe in memory).
- `docs/sail.md` updated, and the Apps compile check above passing.
- The recorder: its round trip green for every phase (see the recorder plan).

### Decisions and questions

- **Ask Mike first** about: the section 1 step shapes where there is a real
  choice; any section 3 runtime bug; the recorder's phase 3 overlay UX. Put
  all the section 1 questions in one proposal and ask once. Keep working on
  unblocked items while waiting.
- **Everything else:** make the best guess, and log it with its reason in
  `DECISIONS.md` in the session scratchpad. It goes to Mike at the end.
- At the end, report what's done, what's open and the decision log, and
  update the memory notes.

## 1. Language and harness gaps (found by Apps)

Each was worked around in the Apps scripts. Sources are the Apps workflow
scripts W1–W8 in the Biz Hub `Apps/src/test/java/sail/apps/` suite.

| # | Gap | Where it hurt | Suggested shape |
|---|---|---|---|
| 1.1 | **Done (39e5c203fd, fb6e39b010):** `upload testSuccess="false"`, then `testFailure`. Was: `upload` can't expect a refusal | Uploading a file that already exists ("file already exists") can't be tested | Let `testFailure` (and `testSuccess`) follow `upload` and check the upload page's or the view's error. Or add an optional `expectFailure`/`message` on `upload`. Prefer reusing `testFailure` |
| 1.2 | **Done (39e5c203fd, fb6e39b010):** `captureCell`, `captureTitle` and `pattern` on the capture steps. Was: `captureValue` reads only a view widget by binding | A ToDo's reference number is shown only in the list and the title | Optional `query`/`column` with `row`/`rowContaining` to read a list cell, and a `title="true"` form (or a separate `captureTitle`) to read the view title |
| 1.3 | **Done (39e5c203fd, fb6e39b010):** `dataGridLink` (PF conformance since P9's fix, 150d05dddc). Was: no step clicks a link in a grid row | W4 can't press "Revoke Access" (a link in each Portal grid row) | Optional `rowContaining`/`row` plus `binding` or grid name on `navigateLink text`, so the link is found within that row. Or a `dataGridLink`/`listGridLink` step |
| 1.4 | **Done (39e5c203fd, fb6e39b010):** `sailWorkflow(...)` (`util.sail.SailWorkflow`) and a `<workflow>` root. Was: `{run}` and `{var:}` don't cross automations | W1, W4, W6 and W6b move between the support centre (PF) and SmartClient, so each is 2–4 scripts that the IT stitches together | A harness runner that takes several scripts (or one file with several automations) and shares the run token and variables between them |
| 1.5 | `viewRoles` can't take a group | Every Apps persona needed its effective roles listed by hand (from SQL) | **Done** (Mike chose C, 2026-11-06, `sail-recorder`): when `sail.recording` is on, `GET sailrecording?roles` returns the signed-in user's effective roles (groups expanded), and the harness builds the views with them after each `<login/>` of a user with no `viewRoles` registration. A registration still wins; without the endpoint (flag off, older server) the views keep every role. See [The test user](../sail.md#the-test-user) |
| 1.6 | SmartClient `dataGridZoom` on a grid without inline editing reported "the grid's button is disabled" unless a `dataGridSelect` came first | W6b, the timesheet's `timedLines` grid | The executor already selects the row before zooming (`dataGridGesture`), so this is probably the zoom being clicked before the button enables. Reproduce in the kitchen sink and wait for the button to enable. Unconfirmed |

### Smaller additions (optional)

- **Done (fb6e39b010):** `{run}`/`{var:}` substitution in `login@customer` and in
  `comment` text. `execute` is left out on purpose, because `{…}` is normal
  JavaScript.
- A child-text form for multi-line values
  (`<dataEnter binding="memo">line 1↵line 2</dataEnter>`). Today a newline is
  written `&#10;`, which works in both renderers and survives marshalling. Only
  worth doing if scripts with long text become painful to read.
- `navigateLink text` is case-sensitive, takes the first link in page order and
  refuses links that open a new window. **Done (fb6e39b010):** `exact="true"`. Still
  open: a way to follow a new window (also recorder gap 10).

## 2. Harness flakes not yet explained

Both came from headed runs of the admin suites (2026-11-05), once each.

- **SmartClient Document Numbers:** a field lost its typed start value after
  Save in `SmartClientAdminSailIT`.
- **PrimeFaces sign-in:** one admin sign-in was refused with correct
  credentials.

Reproduce by looping the affected script headed. Fix with a condition wait in
the executor, not a pause.

## 3. Skyve runtime bugs SAIL found

These are Skyve bugs, not harness bugs. They are reported, not fixed, and the
scripts work around them. Ask Mike which to fix before starting on any, except
SC-9, which is approved. Full
reproductions are in [sail-runtime-issues.md](../sail-runtime-issues.md).

- **P1:** backups are listed oldest first.
- **P2:** restoring a redacted backup locks everyone out.
- **P3:** a tablet action upload never closes or refreshes.
- **P4:** Update My Details fails with an optimistic lock (commented out of
  both admin scripts).
- **P5:** tablet EscapingFixture shows required-field messages at once (may be
  intended).
- **P6:** Re-run Job of Generate Test Data always fails.
- **P8:** an action upload page left open hides later error messages
  (unconfirmed).
- **P9, fixed (150d05dddc), approved by Mike (2026-11-06):** an action link in a
  PF data grid row also zoomed into the row.
- **P10, fixed (150d05dddc), approved by Mike (2026-11-06):** an upload action's
  `ValidationException` showed only a generic error reference.
- **SC-3:** a list without a menu item fails to load.
- **SC-4:** ConditionsFixture never settles.
- **SC-6:** the Backup and Restore "started" growls are not shown.
- **SC-7:** Control Panel jumps back to the Results tab.
- **SC-8:** the User wizard password field loses fast typing.
- **SC-9 (rest), fix approved by Mike (2026-11-05):** an advanced time filter on a list grid with filter
  parameters is still sent as a date, because `completeFilterCriteria` doesn't
  go through `isc.BizUtil.convertFilterCriteria`.
- **SC-10:** selecting a row whose selected handler rerenders loops for ever.
- **SC-11, fixed (approved by Mike, 2026-11-06):** a model lookup column of a
  SmartClient inline data grid row edited again ended the row's edit, because
  `_gather` sorted the grid's data in place. The sort is removed; captures of
  what was sent and shown before and after the change were identical.
- **SC-12, fixed (approved by Mike, 2026-11-06, option C):** a column header
  sort of an unordered SmartClient data grid reordered the domain collection on
  the next request. `ViewJSONManipulator` now follows the request's order only
  for ordered collections.

P7 is by design (Mike, 2026-11-05): the restored selection shows where you came
from after zooming in and pressing OK or Cancel.

## 4. Other loose ends

- One Eclipse null warning: `@Nonnull parentBean` on `WebUtil.addNewElement`.
- Not yet proven in a conformance script: lookup columns in a SmartClient
  inline data grid.
- The language gaps the recorder needs later (recorder gaps 5–10:
  `listGridEdit`, list grid tools, tags and snapshots, `dataGridMove`, actions
  inside trees, maps and calendars, browser back/refresh/new tab and checking
  downloads) are in the recorder plan.
- After this work, Mike reruns the Apps A/B regression test (master vs `sail`)
  from the Apps chat. The recorder (phases 1–5) is merged into `sail` first, so
  the rerun covers it. See [Backward compatibility](#backward-compatibility).
