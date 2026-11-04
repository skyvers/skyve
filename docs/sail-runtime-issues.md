# SAIL Runtime Issues

The SAIL suites (see [SAIL Browser Testing](sail.md)) found bugs in the Skyve
runtime and framework. They are outside SAIL's scope, so they were reported and
not fixed. The scripts avoid them or work around them, as each entry says.

Each suspected cause was checked against the source on the `sail` branch.

- **Confirmed**: the code does what the entry says.
- **Plausible**: the code is consistent with the symptom, but the exact
  mechanism is not proven.
- **Not confirmed**: the code does not show the suspected cause.

Line numbers are for `sail` at the time of writing.

| Id | Summary | Renderer | Cause checked | Status |
|---|---|---|---|---|
| [P1](#p1-backups-are-listed-oldest-first) | Backups are listed oldest first | both | Confirmed | open |
| [P2](#p2-restoring-a-redacted-backup-locks-everyone-out) | Restoring a redacted backup locks everyone out | both | Confirmed | open |
| [P3](#p3-a-tablet-action-upload-never-closes-or-refreshes) | A tablet action upload never closes or refreshes | PF | Plausible | open |
| [P4](#p4-update-my-details-fails-with-an-optimistic-lock) | Update My Details fails with an optimistic lock | both | Partly confirmed | open |
| [SC-1](#sc-1-withdrawn-actions-blocked-after-a-server-field-error) | Actions blocked after a server field error | SC | Withdrawn: a harness fault | withdrawn |
| [SC-2](#sc-2-inline-grid-default-values-reach-smartclient-as-objects) | Inline grid default values reach SmartClient as objects | SC | Confirmed | open on `sail`; fixed on `master` |
| [SC-3](#sc-3-a-list-without-a-menu-item-fails-to-load) | A list without a menu item fails to load | SC | Confirmed | open |
| [SC-4](#sc-4-conditionsfixture-never-settles) | ConditionsFixture never settles | SC | In SmartClient, not Skyve | open |
| [SC-6](#sc-6-the-backup-started-growl-is-not-shown) | The "Backup Job has been started" growl is not shown | SC | Plausible (client) | open |
| [SC-7](#sc-7-control-panel-jumps-back-to-the-results-tab) | Control Panel jumps back to the Results tab | SC | Confirmed | open |
| [SC-8](#sc-8-the-user-wizard-password-field-loses-fast-typing) | The User wizard password field loses fast typing | SC | Confirmed | open |

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

**Workaround in the suites.** 00-backup asserts that no other backup exists,
and 99-restore restores row 0. See [the precondition](sail.md#admin-application-suite).

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

- `skyve-web/src/main/java/org/skyve/impl/web/faces/views/ContentUploadView.java:697-701`
  only queues the success message (`addActionUploadSuccess`).
- A content upload, by contrast, closes its page with a script
  (`ContentUploadView.java:556`, reaching `SKYVE.PF.afterContentUpload` in
  `skyve-web/src/js/prime/pf.js`).
- `skyve-war/src/main/webapp/upload.xhtml:33-37`, the only after-upload hook,
  handles SmartClient only.
- The view refreshes only when the overlay or dialog hides.
  `skyve-web/src/main/java/org/skyve/impl/web/faces/pipeline/component/TabularComponentBuilder.java:4917`
  (dialog) and `:4933` (overlay) set `onHide` to the refresh call.
- On `external`, the page probably goes away through PrimeFaces' own overlay
  auto-hide, not through Skyve code. That would explain why only tablet shows
  the bug, but it is not proven.

A fix would be for `ContentUploadView` to run a script on success that hides the
parent overlay or dialog, which also fires the refresh, as content uploads do.

**Checked:** Plausible. There is no UX/UI-specific code; the
difference between tablet and external is not explained by Skyve code.

**Workaround in the suites.** On tablet, the PF executor closes the upload page
itself when it reports success. The kitchen sink rerenders with a check box
before checking Outcome.

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

## SC-1 (withdrawn): Actions blocked after a server field error

**Reported symptom.** On Communications, after GetCount answered "A tag must be
selected", later validating actions (TestSend, GetResults, even Delete) sent
nothing.

**Finding.** This was a harness fault, not a runtime bug. SmartClient pairs two
clicks on the same widget within its double-click delay into a double click,
and the second action click was lost that way. The SC executor now waits out
the double-click interval when a click would pair, and the actions run.

**Status:** withdrawn.

## SC-2: Inline grid default values reach SmartClient as objects

**Symptom.** In SmartClient, a new row in an inline data grid shows the
`booleanFlag` check box as checked, and the row can be dropped when it is
committed. Colour has the same pattern, in a column that is commented out.

**Reproduction.** Open a kitchensink view with the InlineGrid data grid in
SmartClient, and add a row.

**Suspected cause.**

- `skyve-war/src/main/java/modules/kitchensink/InlineGrid/InlineGrid.xml:34-36`
  declares `<defaultValue>{el:false}</defaultValue>`.
- `skyve-web/src/main/java/org/skyve/impl/web/service/smartclient/SmartClientDataGridFieldDefinition.java:85-101`
  leaves boolean, integer and long default values unquoted, and `:175-176`
  emits `",defaultValue:" + expression`.
- The client therefore receives `defaultValue:{el:false}`, a JavaScript object,
  which is truthy. The expression is never evaluated.

**Checked:** Confirmed on `sail`. Commit `738e964cde` on `master` (not yet in
`sail`) removes the client-side default value. New inline rows are now created
on the server, where default value expressions are evaluated.

**Status:** open on `sail`; fixed on `master`.

## SC-3: A list without a menu item fails to load

**Symptom.** A SmartClient list for a document or query that has no list menu
item throws `Cannot read properties of null (reading 'canCreate')`, and the grid
never shows rows.

**Reproduction.** Open `?a=l&m=kitchensink&q=ModelLookupFixture` in SmartClient.

**Suspected cause.**

- `skyve-web/src/main/java/org/skyve/impl/web/faces/views/DesktopView.java:183`
  (through `listDataSources`, `:719-744`) emits data sources only for the
  module's home list and its menu items.
- `DesktopView.java:228` still asks the list view to use `<module>_<query>`.
- `skyve-web/src/js/desktop/listview.js:321-322` (`_getDataSource`) returns
  `null` for the undefined data source.
- `skyve-web/src/js/desktop/listgrid.js:1956-1958` reads
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

## SC-6: The backup started growl is not shown

**Symptom.** In SmartClient, Data Maintenance > Backup does not show the
"Backup Job has been started" growl. PrimeFaces shows it. Only the later
"Backup Completed" push message appears.

**Reproduction.** In SmartClient, open Data Maintenance > Backup/Restore and
press Backup.

**Suspected cause.** The server side works:

- `skyve-war/src/main/java/modules/admin/DataMaintenance/actions/Backup.java:36`
  calls `webContext.growl(...)`.
- `SmartClientWebContext` keeps the growl, and `ViewJSONManipulator` returns it
  as `_growls`.
- `skyve-web/src/js/desktop/editView.js` passes it to `isc.BizUtil.growl`.

The likely cause is on the client. `skyve-web/src/js/desktop/util.js:853-860`
re-creates one shared PrimeFaces Growl widget for every growl, and PrimeFaces
Growl clears its existing messages when it shows new ones. The backup job's
push message (`skyve-ext/src/main/java/org/skyve/impl/backup/BackupJob.java:601`)
arrives through the same widget (`util.js:867-886`) and can replace the
"started" growl before it is seen.

**Checked:** Not confirmed as a server bug; plausible as a client-side race.

**Workaround in the suites.** The SC suite leaves this `testMessage` out of the
green path, with an XML comment.

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
- `skyve-web/src/js/desktop/editView.js:1431-1436` selects the bound tab on
  every scatter.
- SmartClient's tab selection (`BizTabPane.tabSelected`, `editView.js:2239-2246`)
  does not write the user's choice back to the binding. The server therefore
  keeps `tabIndex` at 2.

**Checked:** Confirmed.

**Workaround in the suites.** The SC suite leaves the tab check after
GenerateMenuSAIL out of the green path, with an XML comment.

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
- When the response is applied (`skyve-web/src/js/desktop/editView.js:962`,
  `this._vm.setValues(...)`), it writes back the value that was sent. This
  overwrites anything typed in the meantime.
- The User edit view (`User/views/edit.xml`) uses `onBlurHandlers` and is not
  affected.

**Checked:** Confirmed.

**Workaround in the suites.** The SC executor types a field with a change
handler one key at a time, and waits for what each key starts.

**Status:** open.
