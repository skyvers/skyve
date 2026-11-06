# SAIL Runtime Issues

The SAIL suites (see [SAIL Browser Testing](sail.md)) found bugs in the Skyve
runtime and framework. They are outside SAIL's scope, so they were reported and
not fixed. The scripts avoid them or work around them, as each entry says.

Each suspected cause was checked against the source on the `sail` branch.

- **Confirmed**: the code does what the entry says.
- **Plausible**: the code is consistent with the symptom, but the exact
  mechanism is not proven.
- **Not confirmed**: the code does not show the suspected cause.

All file and line citations are for `sail` at `f332f66846`. The exception is
SC-2's suspected cause, which cites the code before its fix, at `2f4c2021b8`.

| Id | Summary | Renderer | Cause checked | Status |
|---|---|---|---|---|
| [P1](#p1-backups-are-listed-oldest-first) | Backups are listed oldest first | both | Confirmed | open |
| [P2](#p2-restoring-a-redacted-backup-locks-everyone-out) | Restoring a redacted backup locks everyone out | both | Confirmed | open |
| [P3](#p3-a-tablet-action-upload-never-closes-or-refreshes) | A tablet action upload never closes or refreshes | PF | Plausible | open |
| [P4](#p4-update-my-details-fails-with-an-optimistic-lock) | Update My Details fails with an optimistic lock | both | Partly confirmed | open |
| [P5](#p5-tablet-escapingfixture-shows-required-field-messages-at-once) | Tablet EscapingFixture shows required-field messages at once | PF | Not investigated | open (may be by design) |
| [P6](#p6-re-run-job-of-generate-test-data-always-fails) | Re-run Job of Generate Test Data always fails | both | Confirmed | open |
| [P7](#p7-a-vue-list-grid-restores-a-selection-without-its-selected-action) | A Vue list grid restores a selection without its selected action | PF | Confirmed | open |
| [P8](#p8-an-action-upload-page-left-open-hides-later-error-messages) | An action upload page left open hides later error messages | PF | Not investigated | open (not confirmed) |
| [P9](#p9-fixed-an-action-link-in-a-data-grid-row-also-zooms-into-the-row) | An action link in a data grid row also zooms into the row | PF | Confirmed | fixed (150d05dddc) |
| [P10](#p10-fixed-an-upload-actions-validationexception-shows-only-an-error-reference) | An upload action's ValidationException shows only an error reference | both | Confirmed | fixed (150d05dddc) |
| [P11](#p11-fixed-every-pf-response-moves-the-focus-to-the-first-input) | Every PF response moves the focus to the first input | PF | Confirmed | fixed (COMMIT) |
| [SC-1](#sc-1-withdrawn-actions-blocked-after-a-server-field-error) | Actions blocked after a server field error | SC | Withdrawn: a harness fault | withdrawn |
| [SC-2](#sc-2-fixed-inline-grid-default-values-reach-smartclient-as-objects) | Inline grid default values reach SmartClient as objects | SC | Confirmed | fixed by master |
| [SC-3](#sc-3-a-list-without-a-menu-item-fails-to-load) | A list without a menu item fails to load | SC | Confirmed | open |
| [SC-4](#sc-4-conditionsfixture-never-settles) | ConditionsFixture never settles | SC | In SmartClient, not Skyve | open |
| [SC-6](#sc-6-the-backup-and-restore-started-growls-are-not-shown) | The Backup and Restore "started" growls are not shown | SC | Plausible (client) | open |
| [SC-7](#sc-7-control-panel-jumps-back-to-the-results-tab) | Control Panel jumps back to the Results tab | SC | Confirmed | open |
| [SC-8](#sc-8-the-user-wizard-password-field-loses-fast-typing) | The User wizard password field loses fast typing | SC | Confirmed | open |
| [SC-11](#sc-11-a-model-lookup-column-ends-the-inline-edit-of-a-row-edited-again) | A model lookup column ends the inline edit of a row edited again | SC | Confirmed (stack traced) | open |

SC finding 5 is not listed. It was that a view opened by URL hides Delete, OK
and Cancel, which is by design (`onlyView`); see the note under
[Actions](sail.md#actions). SC finding 9 was the same bug as P1.

## P1: Backups are listed oldest first

**Symptom.** Data Maintenance > Backup/Restore lists the oldest backup in the
first row, although the list is meant to be newest first.

**Reproduction.** Make two backups, then open Data Maintenance > Backup/Restore
and press Refresh. The older backup is row 0.

**Suspected cause.** `BackupsModel.fetchBackups` lists the zip files in
descending name order, then copies them into a `TreeMap`, which sorts its keys
ascending.

- `skyve-war/src/main/java/modules/admin/DataMaintenance/models/BackupsModel.java:228`
  creates the map: `Map<String, Long> backups = new TreeMap<>();`.
- `BackupsModel.java:242` lists the files:
  `FileUtil.listFiles(new File(dirPath), ".*\\.zip", SortDirection.descending)`.
  `skyve-ext/src/main/java/org/skyve/util/FileUtil.java:521-522` really does
  reverse-sort.
- `BackupsModel.java:275` iterates over `backups.keySet()`, in ascending order.

External backups (`BackupsModel.java:229-239`) go into the same map, so they are
ascending too. A `LinkedHashMap`, or a `TreeMap` with `Comparator.reverseOrder()`,
would keep the intended order.

**Checked:** Confirmed.

**Workaround in the suites.** 00-precondition asserts that no backup exists,
00-backup makes the only one, and 99-restore restores row 0. 99-restore is held
back unless the precondition passed. See [the precondition](sail.md#admin-application-suite).

**Status:** open.

## P2: Restoring a redacted backup locks everyone out

**Symptom.** A backup made with a data sensitivity above "none" masks password
hashes, for example `{a**********Nk`. Restoring it succeeds, but afterwards
every sign in throws
`IllegalArgumentException: The name of the password encoder is improperly formatted`
(server.log `UT005023 ... /skyve/loginAttempt`). Sign in does not fail cleanly.
Recovery needs a database reset.

**Reproduction.** Data Maintenance: back up with data sensitivity "Personal",
restore that backup, then sign in.

**Suspected cause.**

- `skyve-war/src/main/java/modules/admin/User/User.xml:19` declares
  `<text name="password" audited="false" sensitivity="secret">`. Secret is the
  highest sensitivity, so every redaction level above none redacts it.
- `skyve-ext/src/main/java/org/skyve/impl/backup/BackupJob.java:229` decides
  `redact = (sensitivityLevel > 0) && (sensitivity != null) && (sensitivity.ordinal() >= sensitivityLevel)`.
  `BackupJob.java:270-275` then calls `BackupUtil.redactData`.
- `skyve-ext/src/main/java/org/skyve/impl/backup/BackupUtil.java` (`redactString`)
  keeps the first two and last two characters. `{argon2}…Nk` therefore becomes
  `{a…Nk`: the opening `{` remains but the closing `}` is lost.
- `skyve-ext/src/main/java/org/skyve/impl/backup/RestoreJob.java` writes text
  columns back as they were read. It has no reference to sensitivity or
  redaction, and the backup does not record its redaction level.
- Sign in uses Spring Security's `DelegatingPasswordEncoder`
  (`skyve-web/src/main/java/org/skyve/impl/web/spring/SkyveSpringSecurity.java:78-79`,
  `skyve-ext/src/main/java/org/skyve/util/SecurityUtil.java:392-399`). It throws
  this exception when a stored hash starts with `{` and has no closing `}`.

Possible fixes: never redact `ADM_SecurityUser.password` (or clear or reset it
instead), or have the restore refuse or warn about a redacted backup.

**Checked:** Confirmed.

**Workaround in the suites.** The suites back up with data sensitivity "none".
The old admin script's `testDataEnter` chose a random sensitivity; that is how
the test server was locked out twice.

**Status:** open.

## P3: A tablet action upload never closes or refreshes

**Symptom.** With an iPad user agent (the `tablet` UX/UI), an upload action's
file uploads, but the upload page stays open showing "Successful ... uploaded"
and the view is not refreshed. With a desktop user agent (`external`), the page
closes and the view refreshes.

**Reproduction.** As tablet, open kitchensink ActionsFixture and press Upload.
Choose a file and press Upload. The page stays open, and Outcome stays empty
until something rerenders the view.

**Suspected cause.** No Skyve code closes the upload page after an action upload
in PrimeFaces, on any UX/UI.

- `skyve-web/src/main/java/org/skyve/impl/web/faces/views/ContentUploadView.java:723-727`
  only queues the success message (`addActionUploadSuccess`).
- A content upload, by contrast, closes its page with a script
  (`ContentUploadView.java:557`, reaching `SKYVE.PF.afterContentUpload` in
  `skyve-web/src/js/prime/pf.js`).
- `skyve-war/src/main/webapp/upload.xhtml:47-51`, the only after-upload hook,
  handles SmartClient only. Master's new upload page (`62e2264e60`) adds
  handlers that run while a file uploads (`:35-40`), but none of them closes
  the page.
- The view refreshes only when the overlay or dialog hides.
  `skyve-web/src/main/java/org/skyve/impl/web/faces/pipeline/component/TabularComponentBuilder.java:4917`
  (dialog) and `:4935` (overlay) set `onHide` to the refresh call. Master's
  `322b0f1e6b` now appends the overlay to the page body (`:4932`). That moves
  where the overlay shows. It does not close it.
- On `external`, the page probably goes away through PrimeFaces' own overlay
  auto-hide, not through Skyve code. That would explain why only tablet shows
  the bug, but it is not proven.

A fix would be for `ContentUploadView` to run a script on success that hides the
parent overlay or dialog, which also fires the refresh, as content uploads do.

**Checked:** Plausible. There is no UX/UI-specific code; the
difference between tablet and external is not explained by Skyve code.

**Workaround in the suites.** On tablet, when the upload page reports success
but stays open, the PF executor closes it as a user would: with a real click on
its close icon. If there is no close icon, the step fails. The kitchen sink
rerenders with a check box before checking Outcome.

**Status:** open.

## P4: Update My Details fails with an optimistic lock

**Symptom.** On the User Dashboard, Update My Details fails with "Failed to
update this information as it was updated by user admin at <the last sign in>
after you looked at it". server.log shows an `OptimisticLockException` in
`UpdateMyDetails`. The failure follows several sign ins (including failed ones)
or a restore since the server started. It is not seen right after a server
start.

**Reproduction.** Start the server. Run the PF admin suite, or sign in with a
bad and then a good password a few times. Then open User Dashboard, change the
mobile number and press Update My Details.

**Suspected cause.**

- `skyve-war/src/main/java/modules/admin/UserDashboard/actions/UpdateMyDetails.java:31-33`
  saves `bean.getCurrentUser()`, the copy retrieved when the dashboard was
  created (`UserDashboardBizlet` → `UserService.currentAdminUser()`). The save
  cascades to the Contact.
- **Sign-in theory: not confirmed.** `skyve-web/src/main/java/org/skyve/impl/web/spring/SecurityListener.java:119-128`
  and `:199` update `ADM_SecurityUser` with SQL. These updates change only
  `authenticationFailures` and `lastAuthenticationFailure`, not `bizVersion` or
  `bizLock`, so they cannot trip Hibernate's version check. Nothing found so far
  stamps the user's `bizLock` at sign in.
- **Restore theory: confirmed in code.** `RestoreJob` never evicts Hibernate's
  shared (second-level) cache.
  - `AbstractSkyveJob` (`skyve-ext/src/main/java/org/skyve/impl/job/AbstractSkyveJob.java:253, 289`)
    calls `evictAllCached()`. That method only clears the session
    (`skyve-ext/src/main/java/org/skyve/impl/persistence/hibernate/AbstractHibernatePersistence.java:1205-1207`).
  - `evictAllSharedCache()` (`AbstractHibernatePersistence.java:1294`) is not
    called.
  - No `Observer.afterRestore` implementation evicts the cache either.
  - The Contact and UserProxy mappings are second-level cached (`region="reference"`
    in `skyve-war/src/generated/java/modules/admin/domain/admin_orm.hbm.xml`).
    After a restore, a cached version can therefore be stale.

**Checked:** Partly confirmed. The restore cause is consistent with the code.
The sign-in cause is not confirmed, and what makes it fail after sign ins alone
is unexplained.

**Workaround in the suites.** Update My Details is commented out of both admin
scripts, rather than worked around with a delay.

**Status:** open.

## P5: Tablet EscapingFixture shows required-field messages at once

**Symptom.** With the tablet UX/UI, the kitchensink EscapingFixture view shows
its required-field messages as soon as it is first rendered. Client-side
validation then blocks a zoom in without sending a request.

**Reproduction.** As tablet, open kitchensink EscapingFixture and press a
zoom-in button before entering anything.

**Suspected cause.** None suggested. This may be intended behaviour. It is
noted because it made the executor's full-page waits time out until the
executor learned to detect a request that client-side validation had blocked.

**Checked:** Not investigated.

**Status:** open (may be by design).

## P6: Re-run Job of Generate Test Data always fails

**Symptom.** Re-running a completed Generate Test Data job fails at once.
server.log shows
`ClassCastException: modules.admin.Job.JobExtension cannot be cast to modules.admin.ControlPanel.ControlPanelExtension`
at `GenerateTestDataJob.java:42`.

**Reproduction.**

1. Control Panel > Design: Generate Test Data.
2. Jobs: zoom into the completed Generate Test Data row and press Re-run Job.

The rerun is listed as failed.

**Suspected cause.** The rerun gives the job the Job bean, not the Control
Panel it was first started with.

- `skyve-war/src/main/java/modules/admin/Job/actions/RerunJob.java:59` calls
  `EXT.getJobScheduler().runOneShotJob(jobToRerun, bean, CORE.getUser())`, where
  `bean` is the Job being viewed.
- `skyve-war/src/main/java/modules/admin/ControlPanel/GenerateTestDataJob.java:42`
  casts `getBean()` to `ControlPanelExtension`.

Any job that expects the bean it was started with is affected in the same way
when it is re-run.

**Checked:** Confirmed.

**Workaround in the suites.** Both 98-destructive files still press Re-run Job,
and count the failed rerun's row in the completed jobs list (`minRows="2"`).
The PF file notes the bug in a comment.

**Status:** open.

## P7 (by design): A Vue list grid restores a selection without its selected action

**Symptom.** In PrimeFaces, a list grid with a `selectedIdBinding` can show a
row as selected when its view opens, although the view's selected id binding
is empty. The actions that need a selection stay disabled. On Data Maintenance,
the backup row shows as selected, but Delete Backup and Restore stay disabled.
Clicking the row unselects it.

**Reproduction.** In PrimeFaces, open Data Maintenance > Backup/Restore with at
least one backup, and select the backup. Leave the view, then open Data
Maintenance again in the same browser session. The backup row shows as
selected, and Delete Backup and Restore are disabled.

**Suspected cause.** The grid keeps its selection in its session state and
shows it again, but only a row click runs the selected action.

- `skyve-vue/src/ListGrid.vue:1240-1241` keeps the DataTable's state in
  session storage (`stateStorage="session"`, keyed by the data source).
  `:1259` binds the selection to that DataTable (`v-model:selection="selectedRow"`).
- `ListGrid.vue:854-856` documents that the selection "is kept in the grid's
  (session) state so it shows when coming back".
- `ListGrid.vue:839-843` (`onRowSelect`) is the only code that runs the selected
  action with the row's bizId. It runs on the DataTable's `row-select` event
  (`:1262`), which a click fires. Restoring the state does not fire it.
- The selected action is what sets the binding. On Data Maintenance it is
  `BackupSelected`
  (`skyve-war/src/main/java/modules/admin/DataMaintenance/views/edit.xml:42`
  and `:54-56`). Delete Backup and Restore are disabled by `notBackupSelected`
  (`skyve-war/src/main/java/modules/admin/DataMaintenance/DataMaintenance.xml:206-210`
  tests `selectedBackupName != null`).
- A click on the restored row fires `row-unselect` (`:1263`, `onRowUnselect`
  at `:848-852`), which sends no bizId and clears the binding again.

A fix would be to run the selected action for a restored selection, or not to
restore the selection of a grid that has a selected action.

**Checked:** Confirmed.

**Workaround in the suites.** `listGridSelect` on a Vue list grid checks
whether the row already shows as selected. If it does, the PF executor clicks
it twice, once to unselect it and once to select it, as a user would have to.
The step fails if the row does not show as selected afterwards. See
`PrimeFacesSelenide.vueListGridSelect`
(`skyve-war/src/test/java/util/sail/PrimeFacesSelenide.java:409`), added in
`54410fc95f`. The PF admin suite selects the backup row in 04-devops and
again in 99-restore.

**Status:** by design (Mike, 2026-11-05). The grid restores its selection so
that, after zooming into a row and coming back with OK or Cancel, the user can
see which row they came from. Selected actions stay disabled until the row is
selected again, which the suites' workaround does.

## P8: An action upload page left open hides later error messages

**Symptom.** In PrimeFaces, an upload action's upload page that is left open
over the view can stop later error messages from showing. In the old admin
script (`pf-admin-sail.xml`), Communications pressed the AddImage upload action
with `<action>` and did not upload anything. The upload page stayed open.
After master's new upload page arrived, the first refused action on the Manage
tab (GetCount) no longer showed its error message, "A tag must be selected for
results".

**Reproduction.** Not reproduced on its own. In PrimeFaces, open a new
Communication, select the Contents tab, press Add Image and leave its upload
page open. Select the Manage tab and press Get Count.

**Suspected cause.** None found. Things that may be involved:

- Master's `62e2264e60` ("Redo image upload Ux") changed the upload page
  (`skyve-war/src/main/webapp/upload.xhtml`).
- Master's `322b0f1e6b` appends an upload action's overlay to the page body
  (`overlay.setAppendTo("@(body)")`, at
  `skyve-web/src/main/java/org/skyve/impl/web/faces/pipeline/component/TabularComponentBuilder.java:4932`).
- The PF executor reads messages from the page's elements, whether or not
  something covers them (`PrimeFacesSelenide.messages`,
  `skyve-war/src/test/java/util/sail/PrimeFacesSelenide.java:1379-1387`). So
  the message was not in the page at all. The open page covering it would not
  explain that.

**Checked:** Not investigated. The only record is the message of `00482bac03`,
which changed the script instead. It is not known whether the cause is in
Skyve or in the harness.

**Workaround in the suites.** `pf-admin-sail.xml` now uploads an image with
AddImage (`<upload action="AddImage" .../>`), so the upload page closes before
the next step. The PF kitchen sink uploads with its upload action in the same
way. The PF admin suite does not press AddImage.

**Status:** open (not confirmed).

## P9 (fixed): An action link in a data grid row also zooms into the row

**Symptom.** In PrimeFaces, clicking an action link in a row of a data grid that
zooms when a row is selected (a grid with no `selectedIdBinding`) zooms into the
row. The link's confirmation shows for a moment, then the row's view replaces
the page, and the link's action does not run. A user cannot use such a link.
Found by the `dataGridLink` conformance (2026-11-06) on the ActionsFixture's
Run From Row link. Biz Hub Apps' Revoke Access link is the same kind of link.

**Reproduction.** In PrimeFaces (tablet), open a new Actions Fixture, select the
Rows tab, add a row and zoom out. Click the row's Run From Row link: the view
zooms into the row.

**Suspected cause.** The link's text is a plain `<span>`:
`TabularComponentBuilder.addCommandLinkText` adds an `outputText` child to the
`CommandLink`. PrimeFaces' data table selects a row on a click whose target
matches `rowSelectorForRowClick`,
`"td:not(.ui-column-unselectable):not(.ui-grouped-column),span:not(.ui-c)"`
(`components.js` of PrimeFaces 13.0.10). The click lands on the link's span,
which has no `ui-c` class, so the row is selected, and selecting a row of such
a grid zooms into it.

**Checked:** Confirmed. With the span given the class `ui-c` (a one-line change
in `addCommandLinkText`, tried in a scratch build only), the PF `dataGridLink`
conformance passes, declined and confirmed.

**Fix.** `150d05dddc`: `addCommandLinkText` gives the text's span the class
`ui-c`, as a PrimeFaces button's text has. Nothing styles `.ui-c` on its own;
PrimeFaces uses it only to skip row clicks and row drags.

**Workaround in the suites.** None needed now: `PrimeFacesKitchenSinkSailIT.rowLink`
runs the row link, declined and confirmed.

**Status:** fixed.

## P10 (fixed): An upload action's ValidationException shows only an error reference

**Symptom.** When an upload action refuses a file by throwing a
`ValidationException` (as whosin's UploadQualifications and admin's
ImportReportSpecifications do), the upload page does not show its message. It
shows "An error occurred while processing your request. Reference: ...", as an
information message, and the server logs the validation as an unexpected error.
Found while adding the refused upload fixture (2026-11-06).

**Reproduction.** Make an upload action throw
`new ValidationException(new Message("..."))`, and upload a file through it in
either renderer.

**Suspected cause.** `ContentUploadView.handleActionFileUpload` catches
`UploadException` (shown as the upload's problems) and
`ActionUnavailableException` (shown as its message). Any other exception,
`ValidationException` included, falls into `catch (Exception e)`, which logs it
with `WebErrorUtil.logUnexpectedAndGetReference` and adds a `FacesMessage`
with the generic message and no severity, so it shows as information.

**Checked:** Confirmed in the code and in the kitchen sink.

**Fix.** `150d05dddc`: `handleActionFileUpload` turns a `MessageException` from
the upload action into the upload's errors (each message, with its bindings as
where), so the upload page shows it as it shows an `UploadException`'s
problems. A content upload shows a `MessageException`'s messages as error
messages. Unexpected failures and unavailable actions are now shown as errors,
not information.

**Workaround in the suites.** None needed now: the ActionsFixture's Upload
action refuses `sail-invalid.txt` with a `ValidationException`, and both
kitchen sinks check the refusal.

**Status:** fixed.

## P11 (fixed): Every PF response moves the focus to the first input

**Symptom.** In PrimeFaces, after nearly every server round trip (an action, a
change that rerenders, a lookup), the page focuses its first input, or its
first invalid input, 50 ms after the response, and scrolls to it. The user's
page jumps, the focus leaves the field they had tabbed to, and on a tablet or
phone the keyboard or a picker that opens on focus pops up. Found while
explaining why a SAIL click straight after an action upload missed its target
(2026-11-06): closing the upload page refreshes the view, and the focus then
scrolled the page under the click.

**Reproduction.** In PrimeFaces, open a new Actions Fixture in a window short
enough that its first check box is out of sight when Event Text is in the
middle. Type into Event Text and press Tab. When Run From Event's response
arrives, the page scrolls back to the first check box, which has the focus.

**Suspected cause.** `WEB-INF/pages/includes/formFocusAndPush.xhtml`, which
every PrimeFaces page's form includes (edit, list, home, map, public,
selfRegister and startup), has a bare `<p:focus />`. PrimeFaces 13's
`FocusRenderer` writes `PrimeFaces.focus()` (or `PrimeFaces.focus('<first
invalid id>')` when there are error messages) each time it is rendered, and
Skyve's ajax actions and rerenders update the whole form, so it is rendered in
each response. `PrimeFaces.focus` focuses from a 50 ms timer.

**Checked:** Confirmed, by logging the page's focus and scroll events, and by
`PrimeFacesFocusSailIT`, which failed before the fix: the response called
`PrimeFaces.focus` once, the page scrolled from 509 to 60, and the focus moved
from the Run From Form Button the user had tabbed to, to the first check box.

**Fix.** COMMIT: the include renders `<p:focus rendered="#{not
facesContext.postback}" />`, so a page focuses its first input only when it is
loaded (a GET). After a postback, including one that fails validation, the
focus stays where it is; Mike chose not to jump to the first invalid field.
PrimeFaces' own focus restore after an ajax update (the element that had the
focus, by id) is unchanged. A form update without the `p:focus` markup is
harmless: the whole form is replaced, and nothing updates the `p:focus` by id.
Skyve has no other `p:focus` or `PrimeFaces.focus` call; PrimeFaces dialogs
still focus their own first input when they open. Every PrimeFaces view change
is a page load, so it still focuses its first input: New and zoom in a data
grid, zoom out and remove (`ActionUtil.redirectViewScopedConversation`), the
list grids' New and zoom (a URL), and OK, Cancel and Delete
(`SKYVE.PF.popHistory`).

**Workaround in the suites.** None needed now. `PrimeFacesFocusSailIT` checks
that a rerender leaves the scroll and the focus where they were, and that a new
view and a zoom into an existing row focus their first input. The harness's
wait for the deferred focus after each response was removed with the fix.

**Status:** fixed.

## SC-1 (withdrawn): Actions blocked after a server field error

**Reported symptom.** On Communications, after GetCount answered "A tag must be
selected", later validating actions (TestSend, GetResults, even Delete) sent
nothing.

**Finding.** This was a harness fault, not a runtime bug. SmartClient pairs two
clicks on the same widget within its double-click delay into a double click,
and the second action click was lost that way. The SC executor now waits out
the double-click interval when a click would pair, and the actions run.
`sc-admin-sail.xml` and the SC suite now run these actions and assert their
refusals, as the PF scripts do.

**Status:** withdrawn.

## SC-2 (fixed): Inline grid default values reach SmartClient as objects

**Symptom.** In SmartClient, a new row in an inline data grid shows the
`booleanFlag` check box as checked, and the row can be dropped when it is
committed. Colour has the same pattern, in a column that is commented out.

**Reproduction.** Open a kitchensink view with the InlineGrid data grid in
SmartClient, and add a row.

**Suspected cause.** These citations are for `2f4c2021b8`, before the fix.

- `skyve-war/src/main/java/modules/kitchensink/InlineGrid/InlineGrid.xml:34-36`
  declares `<defaultValue>{el:false}</defaultValue>`.
- `skyve-web/src/main/java/org/skyve/impl/web/service/smartclient/SmartClientDataGridFieldDefinition.java:85-101`
  leaves boolean, integer and long default values unquoted, and `:175-176`
  emits `",defaultValue:" + expression`.
- The client therefore receives `defaultValue:{el:false}`, a JavaScript object,
  which is truthy. The expression is never evaluated.

**Checked:** Confirmed on `sail` at `2f4c2021b8`.

**Fix.** Master's `738e964cde` ("Fix inline editing double click on
non-editable column and use the server for new inline records") removed the
default value from the SmartClient field definition. New inline rows are now
created on the server, where default value expressions are evaluated
(`SmartClientDataGridFieldDefinition.java:78`). It reached `sail` through the
null-safety merge (`807f0ef4b8`).

**Status:** fixed by master.

## SC-3: A list without a menu item fails to load

**Symptom.** A SmartClient list for a document or query that has no list menu
item throws `Cannot read properties of null (reading 'canCreate')`, and the grid
never shows rows.

**Reproduction.** Open `?a=l&m=kitchensink&q=ModelLookupFixture` in SmartClient.

**Suspected cause.**

- `skyve-web/src/main/java/org/skyve/impl/web/faces/views/DesktopView.java:196`
  (through `listDataSources`, `:724-749`) emits data sources only for the
  module's home list and its menu items.
- `DesktopView.java:241` still asks the list view to use `<module>_<query>`.
- `skyve-web/src/js/desktop/listview.js:321-322` (`_getDataSource`) returns
  `null` for the undefined data source.
- `skyve-web/src/js/desktop/listgrid.js:1959-1961` reads
  `this._dataSource.canCreate` from that null.

**Checked:** Confirmed.

**Status:** open.

## SC-4: ConditionsFixture never settles

**Symptom.** In SmartClient, the kitchensink ConditionsFixture view never
becomes idle. Its `slider1` slider reports "pending overflow operations"
indefinitely, so neither `isc.AutoTest` nor SAIL ever sees the form as done.

**Reproduction.** Open ConditionsFixture in SmartClient and evaluate
`isc.AutoTest.isSystemDone()`. It stays false.

**Suspected cause.**

- `skyve-war/src/main/java/modules/kitchensink/ConditionsFixture/views/edit.xml:140`
  declares `<slider binding="slider1" min="0" max="10" visible="shownByExpression" enabled="allowed" />`.
- `skyve-web/src/main/java/org/skyve/impl/web/service/smartclient/SmartClientViewRenderer.java:2762-2786`
  renders a plain SmartClient `slider` item.
- Skyve's JavaScript has no slider code. The pending state belongs to
  SmartClient's own slider, probably while it is hidden or disabled by its
  conditions.

**Checked:** The cause is in SmartClient, not Skyve. Skyve's part is a plain
item definition.

**Effect on SAIL.** The SC kitchen sink cannot test ConditionsFixture, which is
the only kitchen sink fixture with a membership widget. Membership is tested in
the admin suite instead.

**Status:** open.

## SC-6: The Backup and Restore started growls are not shown

**Symptom.** In SmartClient, Data Maintenance > Backup does not show the
"Backup Job has been started" growl, and Restore does not show "Restore Job has
been started". PrimeFaces shows both. Only the jobs' later push messages
appear: "Backup Completed" and "System Restore complete."

**Reproduction.** In SmartClient, open Data Maintenance > Backup/Restore and
press Backup. Then select the backup and press Restore.

**Suspected cause.** The server side works:

- `skyve-war/src/main/java/modules/admin/DataMaintenance/actions/Backup.java:36`
  and `skyve-war/src/main/java/modules/admin/DataMaintenance/actions/Restore.java:72`
  call `webContext.growl(...)`.
- `SmartClientWebContext` keeps the growl, and `ViewJSONManipulator` returns it
  as `_growls`.
- `skyve-web/src/js/desktop/editView.js` passes it to `isc.BizUtil.growl`.

The likely cause is on the client. `skyve-web/src/js/desktop/util.js:853-860`
re-creates one shared PrimeFaces Growl widget for every growl, and PrimeFaces
Growl clears its existing messages when it shows new ones. The jobs' push
messages (`skyve-ext/src/main/java/org/skyve/impl/backup/BackupJob.java:601`,
`skyve-ext/src/main/java/org/skyve/impl/backup/RestoreJob.java:120` and `:219`)
arrive through the same widget (`util.js:867-886`). They can replace the
"started" growl before it is seen.

**Checked:** Not confirmed as a server bug; plausible as a client-side race.

**Workaround in the suites.** The SC suite's 00-backup and 99-restore comment
out these `testMessage` steps as a "RUNTIME DIFFERENCE". They wait instead for
the job's row in the completed jobs list.

**Status:** open.

## SC-7: Control Panel jumps back to the Results tab

**Symptom.** In SmartClient, after Generate Menu SAIL selects the Results tab,
the user moves to another tab. A later rerender, for example ticking a check
box, jumps the view back to Results.

**Reproduction.** In SmartClient, open Control Panel and run Generate Menu SAIL.
Then select another tab, such as Instrumentation, and tick one of its check
boxes.

**Suspected cause.**

- `skyve-war/src/main/java/modules/admin/ControlPanel/actions/GenerateSAIL.java:124`
  sets `bean.setTabIndex(Integer.valueOf(2))`. `GenerateMenuSAIL` extends
  `GenerateSAIL`.
- `skyve-war/src/main/java/modules/admin/ControlPanel/views/edit.xml:4` binds
  the tab pane: `<tabPane selectedTabIndexBinding="tabIndex">`. The
  Instrumentation check boxes rerender.
- `skyve-web/src/js/desktop/editView.js:1469-1474` selects the bound tab on
  every scatter.
- SmartClient's tab selection (`BizTabPane.tabSelected`, `editView.js:2277-2284`)
  does not write the user's choice back to the binding. The server therefore
  keeps `tabIndex` at 2.

**Checked:** Confirmed.

**Workaround in the suites.** The SC suite's 04-devops selects the
Instrumentation tab again after the check box's rerender, with an XML comment.

**Status:** open.

## SC-8: The User wizard password field loses fast typing

**Symptom.** In SmartClient's User wizard, typing quickly into the new password
field loses characters. Typing at human speed works.

**Reproduction.** In SmartClient, open Users and press New. Go to the user name
and password step and type a long password in one burst (for example by
pasting key events). The field ends up with only part of the text.

**Suspected cause.**

- `skyve-war/src/main/java/modules/admin/User/views/_confirmUsernameAndPassword.xml:54-61`
  gives `newPassword` an `onChangedHandlers` `<rerender>`.
- `skyve-web/src/main/java/org/skyve/impl/web/service/smartclient/SmartClientViewRenderer.java:2628-2634`
  renders a plain SmartClient password item. That item keeps SmartClient's
  default `changeOnKeypress: true`; Skyve's own item types turn it off.
- So every keystroke fires the change handler, which saves the instance to the
  server for the rerender.
- When the response is applied (`skyve-web/src/js/desktop/editView.js:968`,
  `this._vm.setValues(...)`), it writes back the value that was sent. This
  overwrites anything typed in the meantime.
- The User edit view (`User/views/edit.xml`) uses `onBlurHandlers` and is not
  affected.

**Checked:** Confirmed.

**Workaround in the suites.** The SC executor types a field with a change
handler one key at a time, and waits for what each key starts.

**Status:** open.

## SC-9 (fixed): Filtering a time column in the advanced filter fails

**Symptom.** In a SmartClient list grid, any advanced filter clause on a time
column fails with "Please enter a properly formatted value". `equals` can seem
to work, but only when the grid filters its cached rows without asking the
server. The filter row is not affected: it sends simple criteria, and those
serialise a time correctly.

**Reproduction.** In SmartClient, open kitchen sink ListAttributes' List, open
the advanced filter and add a clause on the time column, such as Time greater
than 11:11. Press Filter.

**Cause.** A SmartClient library bug, not Skyve's generator or server.
- `SmartClientAttributeDefinition` gives a time attribute the type `HH24_MI`,
  whose base type is SmartClient's `time`. That is correct.
- The filter builder clause's `getCriterion`
  (`isomorphic130/system/modules-debug/ISC_DataBinding.js`, around line
  111641) flags every Date value as a `logicalDate` unless its field inherits
  from `datetime`. A time value therefore carries both `logicalTime` and
  `logicalDate`.
- `Date.toSchemaDate` (`ISC_Core.js`, around line 23567) checks `logicalDate`
  first, so the criterion is sent as `"value":"1970-01-01"`. The time is lost
  in the browser, and the server's `HH24_MI` converter rejects the value.

**Checked:** Confirmed in the browser and on the wire. The files involved are
identical on master, so this is not a null-safety regression.

**Fix (15aec46f53).** In `isc.BizUtil.convertFilterCriteria`
(`skyve-web/src/js/desktop/util.js`), which every list and pick-list fetch
already passes through, delete `logicalDate` from any Date `value`, `start`
or `end` that also has `logicalTime`. Only this bug produces such a Date. It
needs `skyve-min.js` rebuilt and can only be checked in a browser. Checked on
the wire: a time clause is now sent as `12:30:00.000`, and date and dateTime
clauses are unchanged.

A list grid with filter parameters built its criteria again in
`completeFilterCriteria` from the filter editor's criteria, which had not
passed through this function, so an advanced time filter on such a grid was
still sent as a date. Fixed (68b85f9586) in `BizListGrid`'s `filterData`
(`skyve-web/src/js/desktop/listgrid.js`): it now converts the criteria once,
after completing them, and completes a copy (`isc.clone`, which keeps a
Date's logical flags) because the filter row is given the editor's criteria
back unconverted. Checked on the wire: on the List Grid Fixture's grid with
a filter parameter, `time lessThan 12:30` is sent as `12:30:00.000` beside
the parameter's clause.

**Suites.** The SC kitchen sink filters ListAttributes' time column with
`equals`, `lessThan` and `greaterThan` through the advanced filter, and with
`equals` in a filter row. It filters the List Grid Fixture's PrimeFaces data
table of column types, which has a filter parameter, with `equals` in the
filter row and `lessThan` and `greaterThan` in the advanced filter.

**Status:** fixed.

## SC-10: Selecting a row whose selected handler rerenders loops for ever

**Symptom.** In SmartClient, on the kitchen sink List Grid Fixture, selecting
a row in the `qListGridFixtureFaces` list grid makes SmartClient send about 13
requests a second, without end. That grid has `selectedIdBinding` and an
`onSelectedHandlers` `<rerender>`.

**Reproduction.** In SmartClient, open the List Grid Fixture and click a row
of its faces list grid. Watch the network requests.

**Suspected cause.** Read from `skyve-web/src/js/desktop/listgrid.js`
(`selectionUpdated` → `bizSelected`, and `refresh` deselecting), not stepped
through. The rerender refreshes the view's list grids. The refresh deselects
and reselects the row, and each of those fires the selected handler again,
which rerenders again.

**Checked:** Reproduced with Playwright outside SAIL. `listgrid.js` and
`skyve-min.js` are identical on master, so this is not a null-safety
regression; the fixture only exposed it.

**Workaround in the suites.** The SC kitchen sink does not select a row in
that grid. So `navigateEdit bizId="{var:…}"` and `navigateLink href` with
`{var:}` are run only in PF; the substitution is shared code with unit tests.

**Status:** open.

## SC-11: A model lookup column ends the inline edit of a row edited again

**Symptom.** In a SmartClient inline data grid, a lookup description column
on a model (`model="..."`, not a query) can be set in a new row, but not in a
row that is edited again. Typing a key into it, or opening its drop-down, ends
the row's edit at once: the editor closes and the keystroke is lost. A user
sees the same thing.

**Reproduction.** In SmartClient, open the kitchen sink Model Lookup Fixture.
In "Data-grid model lookups", press New, pick a value in the multi-column
lookup and press Enter. Press Edit, click into the lookup and type a letter.

**Cause (traced in the browser).** The grid's model lookup editor gets a
`pickListProperties.filterData` (`SmartClientAttributeDefinition`
`appendEditorProperties`, "Model lookups in grid editors need the enclosing
view's conversation bean"), which calls `view.gather(false)` for `_c`.
`_gather` (`skyve-web/src/js/desktop/editView.js`) sorts every data grid's
data array in place with `data.sortByProperty(grid._ordinal, true)`, even for
a grid without an ordinal. That fires the grid's `dataChanged`, whose
`_remapEditRows` calls `hideInlineEditor`: the stack from the drop-down's
fetch is `filterDataBoundPickList` → `filterData` → `gather` → `_gather` →
`sortByProperty` → `dataChanged` → `_remapEditRows` → `hideInlineEditor`. Why
a new row's edit survives the same fetch was not looked into.

**Workaround in the suites.** The SC kitchen sink sets the model lookup
columns in a new row only. After `dataGridEdit` it reads them, but does not
change them.

**Status:** open.
