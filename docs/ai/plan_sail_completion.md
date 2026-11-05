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

- **Language changes** follow the phase 2 principle: extend an existing step
  with an optional attribute where the meaning fits, add an op-code only for a
  new gesture, and keep existing scripts unmarshalling unchanged. Regenerate
  the XSD with `XMLMetaData.main` (never edit it by hand; it has a copy in
  `skyve-core/src/test/resources/schemas/sail.xsd`). Each new step gets an
  `Executor` method, a no-op in the `Generator`, both interpreted executors and
  a kitchen sink conformance script.
- **Definition of done** for harness work: the kitchen sink and admin suites
  green 3 times headless and 3 times headed (`-Dsail.headless=false`), in both
  renderers.
- **No fixed sleeps or pauses** in the harness. Wait on a condition only when
  it is needed, and say why in a comment.
- **Disposable servers only.** Always pass `-Dsail.baseUrl`; never point SAIL
  at port 8080. Run SAIL ITs one at a time (`-DforkCount=1`, named tests, the
  `sail-it` profile).
- Build in an rsync'd mirror, not in the worktree (the IDE's compiler
  overwrites `target/classes`). No Spotless.
- Update `docs/sail.md` (op-code tables, PF and SC columns, Known Limitations)
  in the same change.

## 1. Language and harness gaps (found by Apps)

Each was worked around in the Apps scripts. Sources are the Apps workflow
scripts W1–W8 in the Biz Hub `Apps/src/test/java/sail/apps/` suite.

| # | Gap | Where it hurt | Suggested shape |
|---|---|---|---|
| 1.1 | `upload` can't expect a refusal | Uploading a file that already exists ("file already exists") can't be tested | Let `testFailure` (and `testSuccess`) follow `upload` and check the upload page's or the view's error. Or add an optional `expectFailure`/`message` on `upload`. Prefer reusing `testFailure` |
| 1.2 | `captureValue` reads only a view widget by binding | A ToDo's reference number is shown only in the list and the title | Optional `query`/`column` with `row`/`rowContaining` to read a list cell, and a `title="true"` form (or a separate `captureTitle`) to read the view title |
| 1.3 | No step clicks a link in a grid row | W4 can't press "Revoke Access" (a link in each Portal grid row) | Optional `rowContaining`/`row` plus `binding` or grid name on `navigateLink text`, so the link is found within that row. Or a `dataGridLink`/`listGridLink` step |
| 1.4 | `{run}` and `{var:}` don't cross automations | W1, W4, W6 and W6b move between the support centre (PF) and SmartClient, so each is 2–4 scripts that the IT stitches together | A harness runner that takes several scripts (or one file with several automations) and shares the run token and variables between them |
| 1.5 | `viewRoles` can't take a group | Every Apps persona needed its effective roles listed by hand (from SQL) | Accept a group name with its roles declared in the test, or a helper that expands a customer role set. Groups are data, so the test JVM can't look them up |
| 1.6 | SmartClient `dataGridZoom` on a grid without inline editing reported "the grid's button is disabled" unless a `dataGridSelect` came first | W6b, the timesheet's `timedLines` grid | The executor already selects the row before zooming (`dataGridGesture`), so this is probably the zoom being clicked before the button enables. Reproduce in the kitchen sink and wait for the button to enable. Unconfirmed |

### Smaller additions (optional)

- `{run}`/`{var:}` substitution in `login@customer` and in `comment` text.
  Both are harmless. `execute` is left out on purpose, because `{…}` is normal
  JavaScript.
- A child-text form for multi-line values
  (`<dataEnter binding="memo">line 1↵line 2</dataEnter>`). Today a newline is
  written `&#10;`, which works in both renderers and survives marshalling. Only
  worth doing if scripts with long text become painful to read.
- `navigateLink text` is case-sensitive, takes the first link in page order and
  refuses links that open a new window. Consider an `exact` option and a way to
  follow a new window (also recorder gap 10).

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
scripts work around them. Ask Mike which to fix before starting on any. Full
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
- **SC-3:** a list without a menu item fails to load.
- **SC-4:** ConditionsFixture never settles.
- **SC-6:** the Backup and Restore "started" growls are not shown.
- **SC-7:** Control Panel jumps back to the Results tab.
- **SC-8:** the User wizard password field loses fast typing.
- **SC-9 (rest):** an advanced time filter on a list grid with filter
  parameters is still sent as a date, because `completeFilterCriteria` doesn't
  go through `isc.BizUtil.convertFilterCriteria`.
- **SC-10:** selecting a row whose selected handler rerenders loops for ever.

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
  from the Apps chat. Keep the language backward compatible, so the Apps
  scripts keep working.
