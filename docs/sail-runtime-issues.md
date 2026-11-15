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
| [P1](#p1-fixed-backups-are-listed-oldest-first) | Backups are listed oldest first | both | Confirmed | fixed (a74e625b8c) |
| [P2](#p2-fixed-restoring-a-redacted-backup-locks-everyone-out) | Restoring a redacted backup locks everyone out | both | Confirmed | fixed (a74e625b8c) |
| [P3](#p3-fixed-an-action-upload-never-closes-or-refreshes) | An action upload never closes or refreshes | both | Confirmed | fixed (35ee72a540; SC 1d3b95cd0c) |
| [P4](#p4-fixed-update-my-details-fails-with-an-optimistic-lock) | Update My Details fails with an optimistic lock | both | Confirmed (restore); sign ins not reproduced | fixed (a74e625b8c) |
| [P5](#p5-tablet-escapingfixture-shows-required-field-messages-at-once) | Tablet EscapingFixture shows required-field messages at once | PF | Not reproduced | not reproduced; runtime matches Mike's rule |
| [P6](#p6-fixed-re-run-job-of-generate-test-data-always-fails) | Re-run Job of Generate Test Data always fails | both | Confirmed | fixed (a74e625b8c) |
| [P7](#p7-a-vue-list-grid-restores-a-selection-without-its-selected-action) | A Vue list grid restores a selection without its selected action | PF | Confirmed | open |
| [P8](#p8-fixed-an-action-upload-page-left-open-hides-later-error-messages) | An action upload page left open hides later error messages | PF | Confirmed | fixed (35ee72a540) |
| [P9](#p9-fixed-an-action-link-in-a-data-grid-row-also-zooms-into-the-row) | An action link in a data grid row also zooms into the row | PF | Confirmed | fixed (150d05dddc) |
| [P10](#p10-fixed-an-upload-actions-validationexception-shows-only-an-error-reference) | An upload action's ValidationException shows only an error reference | both | Confirmed | fixed (150d05dddc) |
| [P11](#p11-fixed-every-pf-response-moves-the-focus-to-the-first-input) | Every PF response moves the focus to the first input | PF | Confirmed | fixed (e19217194e) |
| [P12](#p12-fixed-pf-ordered-list-memberships-could-not-be-reordered) | PF ordered list memberships could not be reordered | PF | Confirmed | fixed |
| [SC-1](#sc-1-withdrawn-actions-blocked-after-a-server-field-error) | Actions blocked after a server field error | SC | Withdrawn: a harness fault | withdrawn |
| [SC-2](#sc-2-fixed-inline-grid-default-values-reach-smartclient-as-objects) | Inline grid default values reach SmartClient as objects | SC | Confirmed | fixed by master |
| [SC-3](#sc-3-fixed-a-list-without-a-menu-item-fails-to-load) | A list without a menu item fails to load | SC | Confirmed | fixed (c42551e257) |
| [SC-4](#sc-4-conditionsfixture-never-settles) | ConditionsFixture never settles | SC | In SmartClient, not Skyve | open |
| [SC-6](#sc-6-fixed-the-backup-and-restore-started-growls-are-not-shown) | The Backup and Restore "started" growls are not shown | SC | Confirmed (in the browser) | fixed (c42551e257) |
| [SC-7](#sc-7-fixed-control-panel-jumps-back-to-the-results-tab) | Control Panel jumps back to the Results tab | SC | Confirmed | fixed (c42551e257) |
| [SC-8](#sc-8-fixed-the-user-wizard-password-field-loses-fast-typing) | The User wizard password field loses fast typing | SC | Confirmed (a different mechanism, in the browser) | fixed (c42551e257) |
| [SC-10](#sc-10-fixed-selecting-a-row-whose-selected-handler-rerenders-loops-for-ever) | Selecting a row whose selected handler rerenders loops for ever | SC | Confirmed (stack traced) | fixed (c42551e257) |
| [SC-11](#sc-11-fixed-a-model-lookup-column-ends-the-inline-edit-of-a-row-edited-again) | A model lookup column ends the inline edit of a row edited again | SC | Confirmed (stack traced) | fixed |
| [SC-12](#sc-12-fixed-a-header-sort-of-an-unordered-data-grid-reorders-its-collection) | A header sort of an unordered data grid reorders its collection | SC | Confirmed (on the wire and in unit tests) | fixed |

SC finding 5 is not listed. It was that a view opened by URL hides Delete, OK
and Cancel, which is by design (`onlyView`); see the note under
[Actions](sail.md#actions). SC finding 9 was the same bug as P1.

## P1 (fixed): Backups are listed oldest first

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

**Cause found.** As suspected. Name order was not newest first either:
scheduled backups have a prefix (`DAILY_`, `WEEKLY_`, `MONTHLY_`, `YEARLY_`),
which sorts them above the ad hoc backups.

**Fix.** `BackupsModel.fetchBackups` keeps its listing's order in a
`LinkedHashMap`. External backups keep the order of
`ExternalBackup.listBackups()`, which is modified time descending. Local backups
are sorted the same way, by last modified time descending, with the name
descending between backups modified at the same time (a periodic copy keeps its
daily backup's time). Each file's time is read once, before the sort: a backup
being written changes its time, and a comparator that read it again could
break the sort's contract ("Comparison method violates its general contract").
`BackupsModelH2Test` checks both orders, the tie, and times that change while
the backups are listed; each failed before its fix.

**Workaround in the suites: lifted.** 00-precondition no longer asserts that no
backup exists, and 99-restore restores, then deletes, row 0, which is now the
newest: the backup 00-backup made. Both suites now hold the restore back unless
00-backup passed (the PF suite required only 00-precondition). The checks that
assumed a single backup now accept others. Kept, for another reason: the check
that no restore marker is left over, which shows that an earlier run did not
restore its backup. See [which backup is restored](sail.md#admin-application-suite).

**Status:** fixed (a74e625b8c).

## P2 (fixed): Restoring a redacted backup locks everyone out

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

**Cause found.** As suspected. Spring's `DelegatingPasswordEncoder` also throws
for a stored value with no prefix or an unknown one, and its pbkdf2 and scrypt
encoders throw for a hash they cannot decode.

**Fix.** Redaction is unchanged, so passwords stay redacted, and old and new
backups have the same content.

- `SecurityUtil.createDelegatingPasswordEncoder()`, which sign in and
  `EXT.checkPassword` use, returns an encoder whose `matches` returns false,
  with a warning that does not include the stored value, for any stored value
  it cannot use. A sign in with a redacted password is an ordinary bad
  credentials failure.
- `RestoreJob` counts the restored user passwords that have the redacted form
  (`BackupUtil.isRedactedString`; a password hash never holds an asterisk). If
  there are any, the job's log ends with a warning that the passwords were
  redacted and must be reset, and the completion growl says so too.

`SecurityUtilTest` (malformed stored values, including redacted ones) and
`RestoreJobH2Test` (a redacted backup's restore) failed before the fix. In the
UI, after a restore of a backup made with data sensitivity "Secret", signing in
as `admin` failed with "Invalid username and/or password", and the restore
job's log had the warning.

A backup made with a data sensitivity below "Secret" also redacts the user
names (sensitivity personal), so after its restore a sign in does not find the
user at all. The restore's warning mentions only the passwords.

**Workaround in the suites: kept, for another reason.** A restore of a redacted
backup leaves `admin` unable to sign in until its password is reset, by design,
so the suites still back up with data sensitivity "none".

**Status:** fixed (a74e625b8c).

## P3 (fixed): An action upload never closes or refreshes

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

**Checked:** Confirmed (2026-11-15), on every PrimeFaces UX/UI, not only tablet.
Driven with Playwright on `sail` at `b3bc873128`, the upload page stays open
after a successful action upload with an iPad user agent (`tablet`, an overlay),
a desktop one (`external`, an overlay) and an iPhone one (`phone`, a full-screen
dialog), and Outcome stays empty.

So there is no tablet and external difference to explain on the current code.
The only things that close the page without the fix are PrimeFaces': a mouse
press outside an overlay hides it on every user agent (its `onHide` then
refreshes the view), and for a desktop user agent an overlay also hides when
its target's scrollable parents scroll or the window resizes (it only re-aligns
when `PrimeFaces.env.mobile`, which comes from the user agent, is true;
`OverlayPanel.handleViewportChange` in PrimeFaces 13.0.10). Scrolling the window
by 150 pixels after the upload did not hide the `external` overlay, so which of
these closed it in the run that reported the difference is not known.

In SmartClient, confirmed (2026-11-16) on `sail` at `970d4c50f0`: after a
successful action upload, the upload window stays open showing "Successful ...
uploaded", with the view refreshed behind it.

**Fix.** `ContentUploadView.handleActionFileUpload` sends the upload page a
script when an action upload has no problems:
`SKYVE.PF.afterActionUpload(window, true)`, a new function in `pf.js`, hides the
overlay or dialog that holds the page's frame, as `afterContentUpload` does for
a content upload (with `false`, after an upload with problems, it only notes
that the upload ran; see P8). Hiding it runs its `onHide`, which refreshes the view. An
upload with problems (errors, or only warnings) still leaves its page open, as
its message asks the user to review them. In SmartClient the script finds no
PrimeFaces window and does nothing (see SmartClient below). The prime
`skyve-min.js` is regenerated with build.xml's Closure command.

`ActionsFixtureBizPortAndUploadH2Test.aSuccessfulUploadClosesItsUploadPage`
failed before the fix and passes with it;
`aRefusedUploadLeavesItsUploadPageOpen` and `anUnavailableUploadSendsNoScript`
check that refusals do not close the page. `PrimeFacesActionUploadSailIT` uploads with the close icons hidden in
the `tablet`, `phone` and `external` UX/UIs, so only the page itself can close
it, then checks Outcome. Against a server built from `sail` it fails in all
three (the page stays open); with the fix it passes.

**SmartClient (Mike, 2026-11-16: "Upload - yes, close on success too").** The
upload page's `oncomplete` hook (`skyveAfterActionUpload` in
`skyve-war/src/main/webapp/upload.xhtml`) refreshed the view after every action
upload (`saveInstance(true, '_PUSH')` on the window stack's opener), and nothing
closed the window but its close button (`isc.WindowStack.popoff`, which does not
refresh). Now `ContentUploadView.handleActionFileUpload` also sends the page a
callback parameter once the upload action has run, `actionUploadSucceeded`:
`true` when the upload had no problems. The hook refreshes the view as before,
then closes the window (`isc.WindowStack.popoff(false)`) when it is `true`. It
closes the window last, because it reads the opener from the window stack and
closing the window removes the page. An upload with problems (errors, or only
warnings), a refusal before the action runs and an unexpected failure leave the
window open, showing their messages, and the view is still refreshed after
each, as before. Content uploads are unchanged: their own script closes their
window (`isc.BizUtil.afterContentUpload`). So is the BizPort import window
(`bizImport.xhtml`), which is not an upload action. No desktop JavaScript
changed, so `skyve-min.js` is not regenerated.

`ActionsFixtureBizPortAndUploadH2Test.aSuccessfulUploadTellsItsSmartClientWindowToClose`
and `ThemeFaceletsStructureTest`, which checks the hook, failed before the fix
and pass with it; `aRefusedUploadLeavesItsUploadPageOpen` checks that refusals
send `false`, and `anUnavailableUploadSendsNoScript` that a refusal before the
action runs sends no parameter. `SmartClientActionUploadSailIT` uploads a
refused file, whose window must still be open when the step reads the refusal,
then has windows open without a close button and uploads a good file, then
checks Outcome and that no window is open. Against a server built from `sail`
at `970d4c50f0` it fails ("The upload window has no close button"); with the
fix it passes.

**Workaround in the suites.** None needed now. The kitchen sink checks Outcome
straight after the upload. The PF executor still closes an upload page that
stays open after a success with its close icon: Apps' A/B runs drive Skyve
without this fix (master) with this harness, and Apps' Secure Drive workflow
presses an upload action. The executor waits for a closing overlay to finish
closing, so it does not mistake the success growl shown while the page closes
for a page that stayed open: it reads the growl, then the page's state (closed,
closing or open) in one script, and treats the page as stayed open only when it
is shown and not closing. Before it falls back to the close icon it checks
again, so a page that closed itself is never taken for one that stayed open.
(An earlier version read the closed and closing checks separately, and failed
once when the hide transition ended between them.) The SC upload step
(`SmartClientSelenide.upload`) likewise still closes an action upload's window
that stays open after a success with its close button, for the same A/B runs.
With the fix it finds the window closed: the success message and the closing
come in the same response handling, so the step never reads the message from a
window that then closes. `_sail.windowClose` now returns the top window's close
button only when it shows.

**Status:** fixed (35ee72a540); SmartClient fixed (1d3b95cd0c).

## P4 (fixed): Update My Details fails with an optimistic lock

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

**Workaround in the suites.** Update My Details was commented out of both admin
scripts, rather than worked around with a delay.

**Cause found.**

- **Restore: confirmed.** In a PF suite run on the server before the fix, Update
  My Details after the restore failed. server.log:
  `update ADM_Contact ... actual row count: 0`. The Contact cached before the
  restore had a later `bizVersion` than the restored row. `RestoreJobH2Test`
  reproduces it.
- **Sign ins: not reproduced.** Six good and bad sign ins, in other sessions and
  in the same one, left the `bizVersion` and `bizLock` of the user and its
  Contact unchanged (queried in H2), and Update My Details then succeeded.
  01-session's Update My Details after its sign ins passed on the server before
  the fix. Two things explain the report. The time in the message is the
  failing save's own lock, stamped just before the flush, so it is seconds
  after the latest sign in. And every admin suite run ends with a restore, so a
  later run on the same server, without a restart, met the stale cache.

**Fix.** `RestoreJob` evicts Hibernate's shared cache after it writes the
tables, whether the restore succeeds or fails, and before it reindexes (the
reindex would otherwise read stale beans).

**Workaround in the suites: lifted.** Both 01-session files use Update My Details
on the home page, and again after their sign ins. Both 99-restore files use it
after the restore.

**Status:** fixed (a74e625b8c). DataMaintenance's Truncate action deletes with JDBC
and does not evict the shared cache either; it is not changed.

## P5: Tablet EscapingFixture shows required-field messages at once

**Symptom.** With the tablet UX/UI, the kitchensink EscapingFixture view shows
its required-field messages as soon as it is first rendered. Client-side
validation then blocks a zoom in without sending a request.

**Reproduction.** As tablet, open kitchensink EscapingFixture and press a
zoom-in button before entering anything.

**Suspected cause.** None suggested. This may be intended behaviour. It is
noted because it made the executor's full-page waits time out until the
executor learned to detect a request that client-side validation had blocked.

**Mike's rule (2026-11-15).** "Required messages should not display before an
action is run (and only when clientValidation is true)."

**Checked:** Not reproduced (2026-11-15, on `sail` at `b3bc873128`).

- EscapingFixture's first render shows no message of any kind in `tablet`,
  `phone` and `external` (Playwright; the SAIL executor's error selector also
  finds none), nor in SmartClient. The PF kitchen sink's snapshot of the first
  render shows none.
- Pressing its zoom-in button sends a request, and the response brings the
  required messages. Nothing is blocked in the browser: Skyve's PrimeFaces
  views have no client-side validation (no `primefaces.CLIENT_SIDE_VALIDATION`,
  no `validateClient`, and inputs are not `required` for JSF; only their
  required message is set). PrimeFaces' "client validation" is
  `FacesAction.validateRequiredFields`, on the server.
- It runs for an action unless the action's `clientValidation` is false
  (`ExecuteActionAction`, `SaveAction` for Save and OK,
  `ExecuteDownloadAction`); always for zoom in, zoom out and a data grid's New
  (not inline); and for a `<rerender>` event action unless its
  `clientValidation` is false (`RerenderAction`). Nothing else calls it, so a
  first render, a tab change, an upload page's refresh and every other update
  show no required message.

So the runtime already follows the rule, reading a `<rerender>` as an event
action with its own `clientValidation`. That attribute defaults to true, as it
does in SmartClient (`editView.js` `rerenderAction` validates the form before
it saves), and its documentation says it controls "whether form validation is
run first". If Mike means that a rerender should never show required messages,
whatever its `clientValidation`, that is a change in both renderers and to the
attribute's meaning; it is not made.

The executor's detection of a request that client-side validation blocked
(`nothingSent()` in `PrimeFacesSelenide.waitForFullPageResponse`) is removed.
It could only matter when error messages appear with no request, and neither
Skyve's nor Biz Hub Apps' PrimeFaces views validate in the browser.

`PrimeFacesRequiredMessagesSailIT` checks the rule in `tablet`, `phone` and
`external`: no message on EscapingFixture's first render; its Trusted Display
Name action (`clientValidation="false"`) runs without one; OK (client
validation) shows them and does not close the view; and KitchenSink's Date,
whose blur rerenders with `clientValidation="false"`, leaves its empty required
Text without one.

**Status:** not reproduced; the runtime matches the rule (see above for the
rerender reading).

## P6 (fixed): Re-run Job of Generate Test Data always fails

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

**Workaround in the suites.** Both 98-destructive files pressed Re-run Job, and
counted the failed rerun's row in the completed jobs list (`minRows="2"`).

**Cause found.** As suspected. The job record keeps only the identity of the
bean a job was started with (`beanBizId`, `beanModuleName`, `beanDocumentName`,
set in `AbstractSkyveJob`), not its state. A transient bean, such as the Control
Panel, cannot be read back. A persistent one would be read back as it is now
saved, without its transient values. Data Maintenance's backup options, data
refresh options and audit truncation filters are all transient, so a rerun of a
filtered Truncate Audit Log with the bean read back would delete the whole
audit log.

**Fix.** Re-run Job is offered (`JobExtension.rerunnable()`), and run
(`RerunJob`), only for a job that was started without a bean, such as a
scheduled job. It is rerun without a bean, as it first ran. For a job started
with a bean, Re-run Job is hidden, and the action refuses with a warning.
`JobExtensionH2Test` and `RerunJobH2Test` failed before the fix. Jobs started
with a bean that ignore it, such as a reindex started from Data Maintenance or a
job run with Run Job Now, can no longer be rerun.

**Workaround in the suites: lifted.** Both 98-destructive files check that Re-run
Job is not shown for Generate Test Data, and that the completed jobs list has
one Generate Test Data row.

**Status:** fixed (a74e625b8c).

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

## P8 (fixed): An action upload page left open hides later error messages

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

**Checked:** Confirmed (2026-11-15), in Skyve, on `sail` at `b3bc873128`.

- Reproduced in SAIL with the old script's Communications steps (an inline
  probe, not committed): with AddImage's page left open, Get Count's message
  was missing in `external` in both runs, and in `tablet` in two of six runs
  (one more failed with a stale element at the next step, from the same
  cause). In `phone` the upload page is a full-screen modal dialog, so the
  old script cannot click the tabs behind it, as a user cannot.
- In the browser (Playwright), logging each request's source: the Get Count
  request and its response came first, then the upload action's refresh
  remote command (`FacesView.rerender`, which updates the whole form), whose
  response re-rendered the form, and its messages, without Get Count's error.
- A person hits it too. On `tablet`, with AddImage's page open for 1.5 s,
  pressing Save showed "Saved", and the refresh removed it about 0.4 s later.
- The refresh is the upload page's hide handler. The mouse press of the next
  click outside the overlay hides it (PrimeFaces closes an overlay on an
  outside mouse press), but `onHide` runs only when the overlay's hide
  transition (about 0.1 s) has ended. So the refresh is sent after the request
  of the click that closed the page, and its response, the last, re-renders the
  form without that request's messages. In an `external` run whose Save
  response arrived within the transition, no refresh was sent at all (the
  form's update replaces the overlay), which is why it happened only some of
  the time.
- In SAIL, steps follow each other faster than a person can click. A tab click
  during the overlay's show transition does not hide it, so the page stayed
  open until Get Count's own mouse press closed it. That made the race likely
  in SAIL, but the race is Skyve's: the refresh follows the closing click's
  request whenever that request takes longer than the transition.
- The refresh is not needed when nothing was uploaded: the view has not changed.

**Fix.** The upload page's hide handler refreshes the view only if the page's
upload action has run since it was shown. `ContentUploadView` sends the page
`SKYVE.PF.afterActionUpload(window, <close>)` once the upload action has run
(with or without problems; `true` closes the page, P3), which notes the page's
frame; the hide script of an upload action's overlay or dialog
(`TabularComponentBuilder.uploadButton`) calls the refresh only if
`SKYVE.PF.actionUploadRan('<id>')`, which also forgets the note.
`contentOverlayOnShow` forgets it when the page is shown again. A refusal before
the action runs (an unavailable action, an unexpected failure) changes nothing,
so it is not noted. The refresh still runs when a successful upload closes its
page, and when the user closes a page that shows an upload's problems.

What is left: after an upload whose page stays open showing its problems, a
click on another action closes the page, and its refresh can still follow that
action's request. The upload changed the view then, so it must be refreshed.

`TabularComponentBuilderTest` (the overlay's and the dialogs' hide scripts) and
`ActionsFixtureBizPortAndUploadH2Test.aRefusedUploadLeavesItsUploadPageOpen`
failed before the fix and pass with it; `anUnavailableUploadSendsNoScript`
checks that a refusal before the action runs is not noted. In
the browser, Save's message stays after the fix and Get Count's request is the
only one sent. The probe passes in `tablet` and `external`.

**Workaround in the suites.** None needed now. `pf-admin-sail.xml` uploads an
image with AddImage, then presses AddImage again and leaves its upload page
open before the Manage tab's refusals, which must show their messages. Against
a server built from `sail`, `PrimeFacesAdminSailIT` then failed with Get Count's
message missing; with the fix it passes, headless and headed.

**Status:** fixed (35ee72a540).

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
problems. An `ActionUnavailableException` (also a `MessageException`) that the
upload action or an interceptor throws still goes to the refusal of an
unavailable action, as for every other action. A content upload shows a `MessageException`'s messages as error
messages. Unexpected failures and unavailable actions are now shown as errors,
not information.

**Workaround in the suites.** None needed now: the ActionsFixture's Upload
action refuses `sail-invalid.txt` with a `ValidationException` and
`sail-unavailable.txt` with an `ActionUnavailableException`, and both kitchen
sinks and `ActionsFixtureBizPortAndUploadH2Test` check each refusal.

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

**Fix.** `e19217194e`: the include renders `<p:focus rendered="#{not
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

## P12 (fixed): PF ordered list memberships could not be reordered

**Symptom.** In PrimeFaces, an ordered list membership (its collection has
`ordered="true"`) showed no way to put its members in a new order, and when it
was opened again its members were listed in the candidates' order, not the
order saved. SmartClient's member list can be dragged.

**Cause.** `TabularComponentBuilder.listMembership` built every pick list with
`setShowTargetControls(false)`, so the members list never showed its move up
and down buttons. `SkyveDualListModelMap` built the members list by walking
the candidates (the domain values) in their order. Submitting already kept
the members list's order: `gather()` moves the beans to match it, and an
ordered aggregation keeps the order in its join table's list index.

**Fix (a08aa5541d, approved by Mike).** For an ordered membership only:
- `FacesViewRenderer.renderListMembership` works out whether the collection
  is ordered, as `renderDataGrid` does, and passes it to a new
  `ComponentBuilder.listMembership(..., boolean ordered)` overload, whose
  default calls the existing method, so other builders are unaffected.
  `TabularComponentBuilder` shows the pick list's target controls when it is
  ordered.
- `SkyveDualListModelMap` lists the members in the collection's order.

Unordered memberships are unchanged: no target controls, and the members
listed in the candidates' order.

**Tests.** `TabularComponentBuilderTest` pins the target controls on for an
ordered membership and off for an unordered one. The PF kitchen sink moves
OrderedGridFixture's members with `listMembershipMove` (the real move up and
down buttons), saves, reopens the fixture from its list and checks the order.

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

## SC-3 (fixed): A list without a menu item fails to load

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

**Fix (c42551e257).** `DesktopView.preRender` now puts the definition of the
list's data source in the page's UI script, before the list view is given it
(`listDataSourceDefinition`). It is the definition an edit view writes for its
list grids, `if(window.<id>){}else{isc.RestDataSource.create(...)}`, so it does
nothing when the menu defined the data source. It covers a query, a
document's default query and a model, for lists, trees, maps and calendars.

The menu's data source ids (`listDataSources` returns them) are passed to the
renderer as already visited. So it writes nothing for a data source the menu
defined, the list's or a lookup's. A model is built once (reviews, 4e518f6c2e
and 8bac256689). It is written only when the user may fetch the list's rows: when they have
access to the list (query, document or model aggregate access, which comes from
the menu and the router) and can read its driving document. These are the
checks `SmartClientListServlet` makes for every fetch, so the page offers no
list the servlet would refuse. A list the user may not fetch still fails as
before. With access control off, as in the SAIL servers, access is open
anyway.

Unit tests in `DesktopViewTest` (failed before the fix): a query, a document
and a model list get a definition before `setGridDataSource`; a list without
access, or whose document the user cannot read, gets none.

**Suites.** The SC kitchen sink opens ModelLookupFixture's default query and
the query `qModelLookupFixtures` by their URLs (`navigateList`) and finds the
saved fixture in each.

**Status:** fixed.

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

**Effect on SAIL.** The SC kitchen sink cannot test ConditionsFixture, so its
conditions (`visible` and `enabled` expressions) go untested in SmartClient.
Memberships are still tested there: OrderedGridFixture's `members` (an ordered
list membership) is covered by both kitchen sinks, and the admin suite covers
unordered ones.

**Status:** open.

## SC-6 (fixed): The Backup and Restore started growls are not shown

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

**Checked:** Confirmed in the browser. With `isc.BizUtil.growl` and
`isc.BizUtil.onPushMessage` logged and the growl's DOM watched, pressing
Backup showed "Backup Job has been started" 56 ms after the click. The job's
push message "Backup Completed" came 74 ms later and replaced it. PrimeFaces'
`Growl.show()` (13.0.10) removes the messages showing unless the widget's
`keepAlive` is set. PrimeFaces pages are not affected, because they show
pushed growls in a second widget, `pushGrowl` (`skyve-web/src/js/prime/pf.js`).

**Fix (c42551e257).** `isc.BizUtil.growl` (`skyve-web/src/js/desktop/util.js`)
takes an optional fourth argument, `keep`, which sets the Growl's `keepAlive`.
`onPushMessage` passes it, so a pushed growl is added to the growls showing.
Growls from an action's response replace earlier growls, as before. A
second SmartClient widget, as PrimeFaces has, would sit at the same fixed
position, over the first growl.

**Tests.** `SmartClientPushGrowlSailIT` shows a growl as a response does, then
gives the page a pushed growl as the push stream does, and checks that both
are showing. Before the fix only the pushed growl was. The suites' steps below
cannot show the bug reliably. Whether the push comes before the step reads
the growls is a race: on an unfixed server they passed once.

**Suites.** The `testMessage` steps for "Backup Job has been started" in the SC
suite's 00-backup and "Restore Job has been started" in its 99-restore are
back. The suite still waits for each job in the completed jobs list.

**Status:** fixed.

## SC-7 (fixed): Control Panel jumps back to the Results tab

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

**Checked:** Confirmed. Also, `ViewJSONManipulator.visitTabPane` registered the
binding as immutable, so the server ignored any value the client sent for it.
PrimeFaces binds the tab view's `activeIndex` to it, which is written back.

**Fix (c42551e257).** For any tab pane with a `selectedTabIndexBinding`:
- `BizTabPane.tabSelected` (`skyve-web/src/js/desktop/editView.js`) sets the
  tab number in the view's values at the binding, so the next request sends
  it. The view is not made dirty by this, as a list grid's selected id is not.
  A response that selects the bound tab sets the same number.
- `ViewJSONManipulator` registers the binding as mutable, so the server applies
  it. After review (4e518f6c2e), only when it is a document attribute, as PrimeFaces
  writes `activeIndex` only when it is not read only: a getter with no setter is
  only read. And it takes only the index of one of the tab pane's tabs (a whole
  number from 0 to one less than the number of tabs). Other values are ignored
  with a warning. PrimeFaces takes any integer. The index check applies only
  when tab panes are the binding's only writers (8bac256689). A widget that also
  sets it, such as a spinner, has its value applied as usual. Two tab panes on
  one binding take an index of either. Tests in
  `ViewJSONManipulatorApplyJsonTest`:
  - `aSelectedTabIndexTakesOnlyTheIndexOfATab`;
  - `aSelectedTabIndexBindingWithNoSetterIsOnlyRead`;
  - `aSelectedTabIndexBindingAnotherWidgetSetsTakesAnyValue`;
  - `twoTabPanesWithOneSelectedTabIndexBindingTakeAnIndexOfEither`.

Unit tests: the `ViewJSONManipulatorApplyJsonTest` tests above, and
`ViewJSONManipulatorCoreTest.visitTabPaneLeavesASelectedIndexBindingThatIsNotAnAttributeReadOnly`. The client half is checked in the browser by the SC
admin suite.

**Suites.** The SC suite's 04-devops no longer selects the Instrumentation
tab again after the check box's rerender. It checks that the check box is
still shown.

**Status:** fixed.

## SC-8 (fixed): The User wizard password field loses fast typing

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

**Checked:** Confirmed in the browser, but the keys are lost differently. A
burst of 26 keys left only the first in the field, and only one request was
sent. The first key's change handler sent the rerender request.
SmartClient's prompt for that request put up a click mask that took the
focus, so the other 25 keys went to the page's body and were dropped. No
response overwrote them. Keeping the fields edited since the request would
therefore not help.

**Fix (c42551e257).** `SmartClientViewRenderer.visitOnChangedEventHandler` changes
how a typed form item fires its change handler. This applies to a text field
without completion, a password, a text area, a spinner or a colour picker,
when its change handler calls the server (a `rerender` or a `server` action):
- It renders `changeOnKeypress:false`. The handler fires once, when the value
  is committed (the user leaves the field), as PrimeFaces' `change` event and
  Skyve's own date and number items do. So the user can type at any speed.
- The handler's server events run as a blur handler's do: deferred
  (`rerenderBlurryAction`, `doBlurryAction`), with the view's `_blurry` marker
  set. `changeOnKeypress:false` alone lost a click on a button made straight
  after typing: the button's mousedown commits the field, and the request's
  mask takes the click. The `_blurry` handling runs the button's action after
  the request instead.

Other change handlers fire as before. These are handlers with only client-side
actions, which send no request, and items that are not typed into, such as a
combo, a check box or a completing text field. SmartClient ties a combo box's
`changeOnKeypress` to `addUnknownValues`. Skyve's date and number items, which
had `changeOnKeypress: false` already, now also get the deferral for their
server change handlers.

After review (4e518f6c2e, 8bac256689), a gather the user asked for commits the
value being typed into the focused item first, when the item changes on commit
(`gather(validate, true)`, `_commitFocusedItem`), as leaving it would. These
are gathers for an action, a save, a delete, a zoom, a cancel, a check for
changes, an upload or a report. So what they gather, validate and check for
changes includes the typed value, even if the view is left by something that
does not take the focus. SmartClient's `getValues()` already commits a typed
text item, such as the wizard's password, but not a date item's text field. That
gap was older than this fix.

A gather for a background request commits nothing: a list grid's or a pick
list's fetch, a completion, a refresh or a chart. Committing there runs the
item's change handler while the user is still typing.
`SmartClientFastTypingSailIT.aValueTypedButNotCommittedIsGatheredOnlyForTheUser`
types half a date, checks that a background gather leaves it alone, then checks
that a user's gather commits the whole date while the field keeps the focus.

The SC executor still types a field with a change handler one key at a time,
because the Biz Hub Apps scripts rely on the harness as it is.

**Tests.** In `SmartClientViewRendererCoverageTest` (the first failed before the
fix):
- `aTypedItemWithAServerChangeHandlerChangesOnCommitAndDefersItsEvents`;
- `otherChangeHandlersAreUnchanged`.

`SmartClientFastTypingSailIT` types the wizard's new password in one burst of
key events through Selenium and leaves the field with Tab, then checks the
value. It then types the password again and clicks Next at once. The wizard
must move on to its groups step. Before the fix the field kept only the first
key.

**Status:** fixed.

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

## SC-10 (fixed): Selecting a row whose selected handler rerenders loops for ever

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

**Cause (stack traced).** In the browser, with the grid's methods wrapped to log
their stacks, the loop is:
1. The click runs `selectionUpdated(record)`, which sets `selectedFacesId` and
   calls `bizSelected` after `doubleClickDelay`. That calls
   `rerenderBlurryAction`.
2. The response's callback in `saveInstance` scatters and calls
   `refreshListGrids(true, ...)`, which calls `BizListGrid.refresh()`.
3. `refresh()` calls `grid.deselectAllRecords()`, and SmartClient fires
   `selectionUpdated(null)`. That clears `selectedFacesId` and calls
   `bizSelected` again, so back to step 1. SmartClient fires it again on every
   refresh, although nothing is selected after the first.

There were 65 requests in 5 s. The user's selection was lost too: the
deselection cleared the binding. Admin's list grids with a selected server
action avoid the loop with a refresh flag that the action clears, for example
Data Maintenance's `backupsRefreshRequired` and `BackupSelected`.

**Fix (c42551e257).** `BizListGrid.refresh()` (`skyve-web/src/js/desktop/listgrid.js`)
deselects with the grid's `selectionUpdated` switched off, as
`EditView._handleGridSelection` does for a data grid. The user selected
nothing, so the selected event and the binding are left alone. When the rows
arrive (the `filterData` callback), it selects again, also quietly, the row
whose bizId is the view's `selectedIdBinding` value. So the selection is kept.
After review (4e518f6c2e), when that row is known to be gone, the binding is
cleared (still without the selected event), so it does not keep a row the grid
no longer shows. The row is gone when all the list's rows are in the browser and
it is not among them. On a paged list that has fetched only some of its rows,
the row may be on a page not fetched, so the binding is kept and nothing is
selected (8bac256689). If `filterData` needs no fetch, its callback does not run,
so the rows are checked at once instead.
`SmartClientListGridSelectedEventSailIT.aRefreshThatFetchesPartOfAPagedListKeepsTheSelectedId`
makes 100 tagged lookup values, then selects one that sorts after them all, and
refreshes the unfiltered grid. Before the fix the selected id was cleared and the
view became changed. Selecting a row still runs its selected event once. A restored selection does
not run it, as in PrimeFaces ([P7](#p7-by-design-a-vue-list-grid-restores-a-selection-without-its-selected-action)).

**Tests.** `SmartClientListGridSelectedEventSailIT` counts the edit requests
and the grid's selection events. It selects a row of that grid and waits for
SmartClient to settle, with no request pending and no deferred call due. Then it
checks that there was 1 request and 1 selection event, that the row is
selected and that `selectedFacesId` is its bizId. It checks again after a
second settle. Before the fix the step never settled. It then filters the row out
and refreshes the grid. No event runs, and `selectedFacesId` is cleared.

**Suites.** The SC kitchen sink's find script now selects a row in that grid. It
captures `selectedFacesId`, follows the blurb's link, and runs `navigateEdit
bizId="{var:…}"` and `navigateLink href` with `{var:}`, as the PF kitchen sink
does.

**Status:** fixed.

## SC-11 (fixed): A model lookup column ends the inline edit of a row edited again

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

**Fix (d4f3cf87b9, approved by Mike).** `_gather` no longer sorts a data grid's data. It
still calls `reorderData()`, which numbers an ordered grid's rows (their
`bizOrdinal`) in the order they show. The sort was a no-op apart from its side
effect:
- The data it sorted is the grid's own array (checked in the browser: the
  values manager's array is the grid's `getData()`), which is already in the
  order the rows show, so on an ordered grid, sorting by the ordinals that
  `reorderData()` has just written changes nothing.
- On an unordered grid it sorted by a null property, which leaves a stable
  sort's order as it was.
- The server takes the rows in the order they are sent and applies each
  row's `bizOrdinal` (`ViewJSONManipulator`; its own ordinal sort is
  commented out).

The list membership branch's sort, which only runs for an ordered
membership, was first kept, as no ordered membership existed to compare it on.
It was removed too once the kitchen sink had one (see below): its data is the
member list's own array (checked in the browser), already in the order the
members show. `skyve-min.js` is rebuilt.

**No side effects (Mike's acceptance test).** The same gestures were run on
the bundle with the sort and on the bundle without it, capturing the whole
bean each request sent and what each grid showed (its rendered rows, its
selection and its editing row). The two captures were identical, in row order,
`bizOrdinal` and every other field: an ordered grid after rows were added
through their view, after a drag (`dataGridMove`), after a select, after a
remove, on each save and as loaded again; an unordered grid that is saved, on
each save and as loaded; an unordered inline grid after inline adds, a remove
and a select, and after a column header sort (a real click) and the add that
followed; and an unordered list membership on each save and as loaded. Two
runs on the bundle with the sort were identical too, so the capture is
deterministic.

The second comparison, for the membership branch's sort, used the same method
on OrderedGridFixture's ordered inline grid `lines` and ordered list
membership `members`: inline adds, a drag (`dataGridMove`), an inline edit, a
select and a remove in the grid; members added, dragged into a new order in
the member list (a real drag), one removed; saves, the record reopened and
saved as loaded. Two runs with the sort were identical, the run without it
was identical to both, and so was a rerun of the first comparison's cases.

**Suites.** The SC kitchen sink edits a ModelLookupFixture row again with
`dataGridEdit` and changes its model lookup column with
`lookupDescriptionAutoComplete pick`. Before the fix that step failed ("no
inline editor for column selection of data grid rows (is a row being
edited?)"); after it, it passes.
Both kitchen sinks save OrderedGridFixture's ordered membership and reopen it
to check its members' order; the SC kitchen sink also moves, edits and saves
rows of its ordered inline grid and checks their positions once reopened.
These pass with and without the membership branch's sort.

**Status:** fixed.

## SC-12 (fixed): A header sort of an unordered data grid reorders its collection

**Symptom.** In SmartClient, sorting an unordered data grid by clicking a
column header, then saving (or running any action), reorders the domain
collection to the sorted order. The order is a display choice, but the server
moves the beans, so Hibernate sees the collection as changed and rewrites it
on save (for a many-to-many, its join rows are deleted and inserted again).

**Reproduction.** In SmartClient, open the kitchen sink Actions Fixture, add
rows named b, c and a on its Rows tab and save. Click the Name column's header,
then save again: the save's response lists the rows as a, b, c.

**Cause (checked on the wire).** A header sort of a grid with local data
sorts the grid's data array in place, and `_gather` sends that array, so the
request lists the rows in the sorted order. `ViewJSONManipulator.applyJSON`
then makes the collection follow the request: it moves each existing bean to
its index in the request. That is right for an ordered collection, whose order
is what its grid shows, and wrong for an unordered one.

PrimeFaces is not affected: its data grid columns have no sort, and it
applies values through `FacesView` in place.

**Fix (e1cd0310d9, approved by Mike, option C).** `ViewJSONManipulator` follows the
request's order only for an ordered collection (`Collection.getOrdered()`).
For an unordered collection (or an inverse many), existing beans stay where
they are, beans missing from the request are removed, and new beans go on
the end, so a request that only differs in order changes nothing.

**Tests.** Unit tests on `ViewJSONManipulator`: an unordered collection is
not changed at all when only the order differs, keeps its beans in place,
removes missing ones and appends new ones; an ordered collection takes the
request's order and puts a new bean at its position. The unordered ones
failed before the fix. `SmartClientDataGridSortSailIT` sorts the Actions
Fixture's rows by a real header click, saves with a real click and checks the
order in the save's response: a, b, c before the fix, b, c, a after it.

**Status:** fixed.
