# Skyve 10 Changes

Everything new in Skyve 10 since 9.4.4. Skyve 10 branched from master after 9.4.1, and
9.4.2 to 9.4.4 were patch releases off that line, so their fixes (session warnings, error
page hardening, the FreeMarker class resolver, CVE bumps, list grid Popout) are not repeated
here.

**Upgrading?** Read [Upgrading: what breaks](#upgrading-what-breaks) first.

## Headlines

- **New list grid in PrimeFaces.** The Vue list grid is now the default PrimeFaces list
  grid. It has sticky headers, converter-aware filters and paging.
- **One content widget.** `<content/>` replaces `contentLink` and `contentImage` for links,
  images and video, and adds camera capture, cropping and a new upload screen.
  See [Managed Content Widget](content-widget.md).
- **Server-sent events replace WebSockets** for `EXT.push()`. Queues are bounded and stale
  receivers are reaped, so a long-running server no longer leaks push messages.
- **Server-side questions.** Server code can ask the user OK/Cancel or Yes/No part way
  through a request, with `EXT.getDialogs()`. See [Server-side Questions](#server-side-questions).
- **Typed metadata references.** `generateDomain` writes enums for roles, documents,
  queries and jobs (`AdminRole.SECURITY_ADMINISTRATOR`), so a renamed role fails the build,
  not the app. See [Typed Metadata References](typed-metadata-references.md).
- **Inline conditions in views.** `visible="{el:bean.status eq 'rejected'}"` or
  `disabled="{role:admin.Approver}"` works directly in view XML, with no document condition.
  See [Inline Conditions](inline-conditions.md).
- **Locked-down actions.** The server refuses a view action that the view, as rendered,
  gave the user no way to trigger. See [Server-side Action Availability](server-side-action-availability.md).
- **Sign in done properly.** Remember me works per device, Sign out everywhere is available,
  and two-factor sign in works with Remember me.
- **Lists that agree.** Number converters, list filters and date filters now give the same
  results in Java, SmartClient, the Vue list grid and PrimeFaces.
- **SAIL grew up.** SAIL now has a recorder, suites, a load runner, browser control, video
  and evidence pages. See [SAIL](sail.md).

## UI and UX

- **Editorial (PrimeFaces):**
  - The action bar stays fixed under the title bar.
  - Lists have sticky headers.
  - Every view title has an icon.
  - On wide screens the menu collapses with the topbar's menu button, and stays collapsed.
  - A page without a menu uses the full width.
  - A long title stays at two lines, so on a phone it no longer covers the action bar.
- **Phone friendly:**
  - Data grids reflow into cards on narrow screens.
  - Dialogs open full screen on phones.
  - Action bar buttons are centred on phones only.
- **Theming** is reimplemented: one router decides the UX/UI, template and theme for every
  request. See [Theme Resolution](theme-resolution.md) and [Editorial custom colour](editorial-custom-colour.md).
- **User menu, profile and account pages.** Users get a topbar user menu, and `UserDashboard`
  becomes their profile. The new `admin.UserAccount` page shows their sign-in history and
  sessions.
- **Combo boxes in PrimeFaces** can now be searched as you type.
- **Maps:** Leaflet 1.9.4 with marker clustering at every zoom level, a geodesic helper
  (`GeodesicUtil`), and fixed OpenStreetMap tiles and z-index.
- **Lookups:** a `lookupDescription` can be backed by a list model (`model="..."`). See
  [Model-backed Lookup Descriptions](#model-backed-lookup-descriptions).
- **SmartClient:**
  - Inline grid New now asks the server for the row, so defaults, `newInstance()` and
    interceptors run.
  - Grid action links behave like buttons: they confirm and validate first.
  - Server failures show growls and messages, as PrimeFaces does.
  - Grids redraw faster while scrolling.
  - Disabled fields are easier to read.
- **Environment banners** now also show on the sign-in, error and other JSP pages.
- **German** translation added.
- New `Decimal10` converters: `Decimal10Integer`, `Decimal10IntegerPercentage`,
  `Decimal10OneDecimalPlace` and `Decimal10TwoDecimalPlacesPercentage`.
  `Decimal10DollarsAndCents` can now be named in metadata.

## Content, images and media

- **Camera and crop:** take a photo straight into an image field, then crop it with
  Cropper.js, which replaces Croppie. Uploads are scaled down to fit the maximum upload size.
- **Image markup** is a mobile-friendly Excalidraw-based editor (svg-edit is gone). Markup
  is saved as standard SVG, sanitised, and text-indexed.
- **Video:** `content` attributes play video in the browser. Video uploads have their own
  whitelist and a 100MB default limit (`uploads.video`).
- **Streamed, range and resumable downloads** from every Skyve download endpoint.
- **Thumbnails:** fewer stored sizes, sharper images, bounded memory, and a 503 response
  instead of an endless wait under load. Stored thumbnails are cleaned up when an
  attachment is removed and by garbage collection. See [Thumbnails](#thumbnails).
- **Content garbage collection** walks the file system content store, so it works without
  a search index. Lucene content ids are now exact, so attachments are found, replaced and
  removed reliably. **This needs a full reindex.** See [Lucene content ids](#lucene-content-identifiers-and-garbage-collection).
- **File suffixes on disk:** `content.fileSuffixes` stores attachments with their extension,
  for virus scanners and people browsing the store.
- Attachments and mail attachments accept any content type. Content-Disposition file names
  are sanitised.

## Security and sign in

- **Remember me per device.** Signing out ends Remember me in that browser only.
  See [Remember me and signing out](#remember-me-and-signing-out).
- **Sign out everywhere.** An administrator can sign out all of a user's sessions and
  devices. A user can sign out of all their other sessions. Disabling, deleting or renaming
  a user removes their tokens and ends their sessions.
- **Two-factor sign in with Remember me** issues the cookie, so a restarted browser needs no
  new code. A mistyped code no longer clears Remember me. Codes can't be resent faster than
  `account.tfaResendCooldownSeconds`.
- **MFA fixes:** lockout counting and threshold, second-precision timeouts, a message when a
  code has expired, and the reset-password link is hidden on the code page.
- **Password changes need the old password** (only a reset by email token skips it). A
  disabled user's reset link reads as expired.
- **Session revocation is enforced on every request**, and simultaneous sign-ins of one
  user each keep their session.
- **Hardening:**
  - Restores are protected from zip bombs (`backup.restoreMaxExtract*`).
  - XEE fixed in the codified configuration.
  - Logs are sanitised against log injection (`SkyveLoggerFactory`).
  - View boilerplate is escaped (see [View Boilerplate Escaping](view-boilerplate-escaping.md)).
  - MetaDataServlet JSON is escaped, and the error page's "try again" link breaks out of
    frames.
  - REST list and query pages are capped at 100 rows.
  - The IP address is captured for failed sign-ins.
  - Legacy SHA1 password hashes are removed.
- Microsoft sign in is now labelled Entra ID.

## Data, lists and persistence

- **List filters through associations** now left outer join, so negations, `isNull` and
  Match Any keep the rows that have no association. Three `nullOr...` filter operators that
  had all compared as "greater than" since 2019 now compare as named.
  See [Converters and list filters](#converters-and-list-filters).
- **Number converters are strict and locale proof.** They read the whole text, and always
  read and write English numbers, whatever the JVM's locale.
- **Document numbers** survive concurrent first use, start after the highest existing value,
  and increment digit runs of any length. Skyve warns at startup if the database isn't
  READ COMMITTED.
- **Paged, not streamed, result sets** for all relational access.
  `factories.connectionCustomiserClass` customises each connection for the current user.
- New `Persistence.getEntityManager()`, `SQLDataAccess.commit()` and `rollback()`, and a
  `generated` element on scalar attributes that reaches the Hibernate mapping.
- A coincident extension strategy for mapped documents, with stricter polymorphic
  reference checks.
- **Scheduled backups** make weekly, monthly and yearly copies once per period. Azure copies
  are made server-side, and the job log shows problems and disk usage.
  See [Scheduled Backups](#scheduled-backups-and-azure-copies).
- **Custom properties** keep their declaration order, and fluent builders get
  `putProperty`. See [Custom Property Order](#custom-property-order-and-fluent-builders).
- JSON reads and writes Java records, and decodes Unicode escapes correctly.
- The EL evaluator handles enums.

## Platform and APIs

- **Pluggable services on `EXT`:** `getMailService()`, `getSMSService()` and `getDialogs()`.
  Each can be configured under `factories` (`mailServiceClass`, `smsServiceClass`,
  `dialogsClass`).
- **Mail Log:** every outbound mail is logged, whatever service sends it, and can be
  archived. It can also keep a body excerpt and the attachments.
  See [Mail Log](#mail-log-body-excerpts-and-attachments).
- **`Observer.dataTierReady(Customer)`** is called once the data tier is up, with a
  super-user persistence available.
- **Jobs:** `JobSchedule` for job and report scheduling, and `Job.shouldBeSilent()`. A job's
  persistence is always closed.
- **Push configuration:** the `push` stanza sets keep-alive, queue size, per-user and total
  receiver limits, and the stale receiver timeout.
- **No more JVM switches** for Skyve's Byte Buddy proxies, which now use `MethodHandles`.
- `AbstractResourceServlet` can be extended to serve your own resources and thumbnails.
- **Upgraded libraries:** JasperReports 7, OmniFaces 4.7, Spring Security 6.5.9, Tika 3.3,
  Jackson 2.18 and FreeMarker 2.3.35. A Hibernate 5.6 CVE (CVE-2026-0603) is mitigated.

## Admin module

- **Monitoring dashboard:** request, query and document timings, CPU and RAM, with roll-ups
  and purge.
- **Heap dumps** can be made and downloaded from the admin module.
- **Communications:** bulk import and export.
- Restores warn about redacted user names as well as redacted passwords.
- An admin spruce-up: the audit list shows only the features that work, and
  `ReportDesign` is gone.

## Developer tooling

- **Null-safety:** `jakarta.annotation.Nonnull` and `Nullable` are used throughout the
  public API. Metadata the XSD requires is checked when it loads, and blank required values
  are refused.
- **Domain generation:**
  - Customer overrides get their own generated classes (see [Typed Metadata References](typed-metadata-references.md)).
  - Metadata validation can be switched off.
  - A `Customisations` class can be passed in.
  - Maven plugin goals take command-line properties.
  - Generated action tests support injection.
- **Testing:**
  - JUnit 4 is gone.
  - There is a JaCoCo coverage profile.
  - Deployed integration tests can be reused. See [Deployed Integration Testing](deployed-integration-testing.md).
- **SAIL:**
  - A point-and-click recorder for SmartClient and PrimeFaces.
  - Suites, workflows, tags and tiers, and a `SailLoad` load runner.
  - Browser control and 25 standard calls (`Offline`, `Throttle`, `FailRequests`,
    `SwitchTab`, `StartDevice` and more).
  - Per-member browsers, device profiles, step snapshots and video, and HTML evidence pages.
  - See [SAIL](sail.md).

## Removed

- `skyve-ejb`, `skyve-quarkus`, kickstart and `JPADomainGenerator`.
- WebSockets (`SkyveSocketEndpoint`), replaced by server-sent events.
- The `contentLink` and `contentImage` view widgets, replaced by `<content/>`.
- The `external` PrimeFaces layout family, a copy of `editorial`.
- svg-edit and Croppie. The old `fileUpload`, `imageUpload` and `contentUpload` pages are
  now one `upload.xhtml`.
- `device.jsp` and `device.xhtml` device emulation, and `SetUxUi`.
- `ThumbnailFilter`, `SkyveLegacyPasswordEncoder`, `MailUtil` (use `EXT.getMailService()`),
  `BeanShellUtil`, the Selenese generators, and the admin `ReportDesign` document.

## Upgrading: what breaks

Work through this list. Each item links to the detail.

1. **Rebuild the Lucene content index.** Old indexes must not be reused. With
   `content.fileStorage: false` your attachments live in the index, so back them up first.
   See [Lucene content ids](#lucene-content-identifiers-and-garbage-collection).
2. **Replace `contentLink` and `contentImage`** with `<content/>`. Old views won't load.
   See [Content widget migration](content-widget.md#migration).
3. **Implement `Observer.dataTierReady(Customer)`** in your observers. It's a new abstract
   method.
4. **Re-run `generateDomain`** and re-assemble the admin module.
   - Customer override classes are renamed from `<Document>Ext` to `<Document>Vanilla` and
     `<Document>Override`.
   - Customer domain generation always runs.
   - `Module.getDocument` throws `MetaDataException`.
   - See [Typed metadata: Upgrading](typed-metadata-references.md#upgrading).
5. **Templates and theming:** a copy of `templates/external/` is ignored, and the request
   selection APIs have been removed. See [The `external` family](#the-external-layout-family-is-gone)
   and [Theme Resolution](theme-resolution.md#downstream-descriptor-migration).
6. **Remove any `ThumbnailFilter` mapping** from `web.xml`, or the app won't start.
7. **Check list filters and converters:**
   - Lists through associations can show more rows.
   - Three `nullOr...` operators now compare as named.
   - Number parsing is strict and English.
   - See [Converters and list filters](#converters-and-list-filters).
8. **Check `yearlyBackupRetention`.** `YEARLY_` backups are now culled to it.
9. **SmartClient:**
   - Inline New and grid action links now run server code and validation that they skipped
     before.
   - `SmartClientDataGridFieldDefinition.defaultValueJavascriptExpression` is gone.
   - See [SmartClient behaviour](#smartclient-behaviour).
10. **Remember me:** signing out no longer signs out a user's other devices. An app with its
    own remember-me configuration keeps the old behaviour.
    See [Remember me and signing out](#remember-me-and-signing-out).
11. **Thumbnails:** they are never enlarged past the image's own size, and the browser
    caches content for a day. See [Thumbnails](#thumbnails).
12. **Test your reports** on JasperReports 7, and drop any JVM `--add-opens` switches you
    added for proxies.
13. **SAIL:**
    - A script that passed while signed out now fails.
    - A `<call>` that throws `TestAbortedException` is skipped, not failed.
    - Device profiles have moved.
    - See [SAIL](sail.md#remember-me-and-signed-out).

---

## Details

### Model-backed Lookup Descriptions

```xml
<lookupDescription binding="contact" descriptionBinding="name" model="ContactsModel" />
```

- Works in SmartClient and PrimeFaces, in forms and data grids.
- The model belongs to the document of the enclosing view, even inside a data grid or with
  a compound binding. Customer overrides are supported.
- The model's driving document must match the lookup's target document, and its columns
  must support the description and the drop down.
- Use `query` or `model`, never both. Leave both out and the lookup uses the association's
  query, then the target document's default query, as before.
- A model lookup generates `UserAccess.modelAggregate` access and needs read permission on
  the driving document. If view access generation is off, declare the access yourself.
- See the [lookup model docs](view_models.md#lookup-descriptions-backed-by-list-models), or
  try **Kitchen Sink → Model Lookup Fixture** with these
  [test steps](../skyve-war/src/main/java/modules/kitchensink/ModelLookupFixture/README.md).

### Server-side Questions

```java
boolean charge = EXT.getDialogs().confirm(
		Question.yesNo("Invoice.lateFee", "Late fee", "Charge a late fee of " + fee + "?"));
EXT.getDialogs().ask(Question.okCancel("Invoice.post", "Post", "Post this invoice?").cancelAborts());
```

- You can ask from:
  - custom actions
  - OK, Save, Delete, ZoomOut, rerenders and, in PrimeFaces, Remove
  - their Bizlet callbacks and interceptors
- While a question has no answer, `ask` throws, the request rolls back and the client shows
  the question. The client then resends the request with the answers so far.
- **The code runs again from the start on each resend.** So don't ask after a commit or after
  a side effect outside the transaction, such as an email, a push or a content write.
- You can't ask:
  - on form load or while the response renders
  - from `postRender`
  - from list grids, jobs or background tasks

  There `canAsk()` is false and `ask` throws `IllegalStateException`.
- Try **Kitchen Sink → Dialogs Fixture**.

### Lucene Content Identifiers and Garbage Collection

**What changed:**

- An attachment's `contentId` is now indexed as one exact, stored value. It was tokenised
  text, split at the hyphens of the UUID.
- The old mapping meant an attachment could fail to be found, updated or removed, and a
  replacement could leave its old index document behind.
- An attachment has a `contentId` field and `BeanContent` has none, which is how garbage
  collection and truncation tell them apart.
- Only the Lucene schema changes. Database content ids, the stored files and the Elastic
  content manager are unchanged.

**Garbage collection** still protects:

- content with no modification time
- content newer than `content.gcEligibleAgeMinutes`
- content owned by persistent rows
- attachments referenced elsewhere, including in dynamic fields

It removes orphaned attachments from the index and `SKYVE_STORE`, and orphaned
`BeanContent` left by direct SQL. A failed lookup or removal is logged without stopping the
other candidates, and each type has a safety limit. Garbage collection runs on the Quartz
schedule or from the manual Data Maintenance action.

**This is a breaking index change.** Don't reuse an index written by an earlier Skyve
version. Don't run old and new nodes against the same content directory. Deleting
individual documents, or letting new ones pile up beside the legacy schema, is not a
supported migration.

**With `content.fileStorage: true`** (`SKYVE_STORE` holds the attachments, and
`SKYVE_CONTENT` is a disposable index):

1. Back up the database and the whole content directory, and verify the backups. Check
   that the payload and metadata files are under `SKYVE_STORE`.
2. Stop traffic, every node, and any job that writes or garbage collects content. Upgrade a
   cluster's nodes together.
3. Deploy Skyve 10 in maintenance mode, on the same database and content directory.
4. In **Admin > Data Maintenance**, run **Drop Indexing** once. This removes
   `SKYVE_CONTENT` only, never `SKYVE_STORE`.
5. Run **Reindex** for every customer and wait for each to finish. **Reindex Content**
   alone does not rebuild `BeanContent`.
6. Check the logs. Then check downloads, attachment search and bean content search for
   every customer.
7. Only then re-enable garbage collection and traffic. Keep the backup until the rebuilt
   index and a garbage collection run have both been checked.

**With `content.fileStorage: false`** the attachment bytes are in the index, so **Drop
Indexing destroys them**. Before upgrading, use the old Skyve version to make and verify a
backup with content, or migrate to file storage. Then restore into a clean Skyve 10 content
store. Rehearse on a production-sized copy first.

### Scheduled Backups and Azure Copies

- Weekly, monthly and yearly copies are made once per period, not every day.
- Azure copies blobs server-side instead of streaming them through the app server.
- Plain `YEARLY_` backups are now culled to `yearlyBackupRetention`. They were never culled
  before, so check that setting before the first run.
- Re-assemble the admin module to pick up the job change.

### Custom Property Order and Fluent Builders

- `DecoratedMetaData.getProperties()` iterates in declaration order. Replacing a value
  keeps its position.
- Marshalled XML and the `MetaDataServlet` JSON follow that order.
- Every fluent builder that carries custom properties has `putProperty(name, value)`, and
  `from(...)` copies them. For a role, that includes its document, action and content
  privileges and restrictions.
- A `<property>` without a `key` is ignored.

### Mail Log Body Excerpts and Attachments

Both settings are off by default. They live in the `smtp` stanza and can be changed at
runtime from the Startup page.

- **`smtp.logBodyExcerpt`** stores the first 500 characters of the plain-text body.
  - Only numeric codes after phrases such as "verification code" are masked. Anything else,
    such as a temporary password in a custom template, is stored as written, so check your
    templates first.
  - The column widens from 255 to 500 characters through DDL sync.
- **`smtp.logAttachments`** stores the attachments as a zip, up to
  `smtp.logAttachmentsMaxBytes` (5MB by default).
  - Attachments over the limit are listed in `SKIPPED.txt` inside the zip.
  - The zip is stored unmasked, and anyone who can read the Mail Log can download it
    (SecurityAdministrator by default).
  - Archived Mail Logs drop their attachments.
- `DocumentConverter.prepareForArchive(Bean)` lets a converter clear values before a bean
  is archived.
- `MailLog` gains `getAttachments()` and `setAttachments(String)`. Re-assemble the admin
  module.

### Thumbnails

- **Size:** a thumbnail fits a square of the larger requested side, rounded up to 64, 128,
  256 and so on.
  - The cap is the new `thumbnail.maximumDimension` setting, 2048 by default.
  - A thumbnail is never smaller than the one asked for, and never enlarged past the
    image's own size.
  - An `<img/>` with a CSS width and height and `object-fit:contain` looks the same. One
    without them can show a different size.
  - `Util.getContentImageUrl()` now writes all three.
- **Previews:** forms and data grids ask for a size from their box. SmartClient allows for
  the screen's pixel ratio, so previews are sharp on high resolution screens.
- **Memory:**
  - A thumbnail is decoded at once to twice its own size. That is 16MB at most up to 1024
    pixels, and about 64MB at 2048.
  - An image over 256 megapixels is not read at all: it shows its file type icon.
- **Load:**
  - `thumbnail.concurrentThreads` defaults to `0`, meaning half the processors. Either way
    it is limited to a quarter of the maximum heap.
  - Other requests wait their turn. One that can't start within 60 seconds gets a 503 with
    `Retry-After`.
  - Stored thumbnails are served without waiting.
  - Still rate limit `content`, `resources` and any thumbnail servlet of your own at the
    reverse proxy.
- **Requests:**
  - `_w` or `_h` alone now sizes by that side.
  - `_nm` always returns the original.
  - Content responses are cached for a day (`Cache-Control: private, max-age=86400`), so
    code that writes new bytes into an existing content id can show the old image for a day.
- **Storage:**
  - Thumbnails are written atomically.
  - Nothing is stored when the drive has less than 10% free.
  - A file's thumbnail is remade when the file changes.
  - Removing an attachment deletes its thumbnails. Code that removes attachments without a
    `ContentManager` should call `Thumbnail.deleteStoredThumbnails()`.
  - Garbage collection deletes thumbnails over 90 days old once a day.
- **Fixes:**
  - File type icons keep their shape.
  - SmartClient thumbnail columns keep tall images inside the column.
  - Dynamic images take their width from the pixel width.
  - Contact avatars are cropped to the circle instead of stretched.
- **Removed:** `ThumbnailFilter`, added in 9.4.0 and never used by Skyve. To serve your own
  files' thumbnails, extend `AbstractResourceServlet`.

### Converters and List Filters

The rules are in the Converters subsection of [SAIL](sail.md). What changes:

- **Number parsing** reads the whole trimmed text and refuses:
  - trailing text (`12abc`)
  - exponents (`1E3`)
  - `+` on a decimal
  - digits other than 0-9
  - a `,` that isn't between digits

  A value that rounds to zero has no sign (`0.00`).
- **English numbers whatever the JVM's locale.** This covers the converters,
  `DecimalFormatter` and the formatters. A German JVM used to write `1.234,50`.
- **Data file imports** still read the number at the start of a cell (`1,234.50 kg`), and
  read spreadsheet cells as English text.
- **Durations** round the minutes once, half up (2.4999 hours is `2:30`), and `-0:30` reads
  as -0.5.
- **Negations** (not equal, not contains, not in, Match None and so on) now list the rows
  with no value, as SmartClient does. Match None negates a nested group as a whole, and
  not in an empty set lists every row.
- **Joins through associations:**
  - In a list built on a document query, every filter condition through an association now
    left outer joins its path. Before, an implicit inner join dropped the rows with no
    association.
  - The count matches the rows, and sorting on the association keeps those rows.
  - Under Match All, a positive condition through the association still leaves them out.
  - A custom `from` that joins the association itself keeps its own join.
  - SQL, in-memory and archived list models are unchanged.
- **Bug fix:** the `nullOrGreaterEqual`, `nullOrLess` and `nullOrLessEqual` filter
  parameters all compared as "greater than" since 2019. They now compare as named.
- `between` now excludes both ends, as SmartClient defines it. `iBetween` used to include
  them.
- **SmartClient:**
  - 12-hour times show upper-case `AM` and `PM`.
  - A duration refuses decimal hours without a colon.
  - A date time shown as a date filters by the whole local day.
  - The absolute dollars column filters by signed dollars.
  - A refused filter number shows an error.
- **PrimeFaces** date pickers read `am` or `pm` in either case. **Vue list grid** date
  filters take the column's converter format.

### Remember Me and Signing Out

- **Per device:** Skyve's `SpringSecurityConfig` uses `SkyveRememberMeServices`, which
  removes only the series in the signing-out browser's `remember` cookie. A password change
  still removes all of a user's tokens. No schema change.
- **Ending Remember me on every device:** use any of these.
  - **Sign out of all other sessions** on the user's own account page.
  - **Sign out everywhere** on `admin.User` (for an administrator).
  - A password change.
  - Delete the user's rows in `ADM_UserToken`.
- **Your own remember-me configuration** keeps signing out everywhere. To take the change:
  - Use `SkyveRememberMeServices(key, userDetailsService, tokenRepository)` with a
    `SkyveRememberMeTokenRepository`, set up as `SpringSecurityConfig.rememberMeServices()`
    does.
  - If your cookie isn't named `remember`, don't offer the account page's button.
- **New API:**
  - `StateUtil.removeOtherSessions`
  - `SkyveRememberMeTokenRepository.removeUserToken` and `removeOtherUserTokens`
  - `SkyveRememberMeServices.extractSeries`
  - the public `SpringSecurityConfig.REMEMBER_ME_PARAMETER`
- The new actions need privileges in your roles, and there are new i18n keys:
  `admin.user.actions.signOutEverywhere.*` and `admin.userAccount.actions.signOutOtherSessions.*`.

### SmartClient Behaviour

- **Inline grid New** posts the form, and the server creates the row.
  - Defaults, `newInstance()`, interceptors and `preExecute(ImplicitActionName.Add)` now
    run.
  - An interceptor veto stops the add.
  - A row that fails validation is kept, and no row is added.
  - New also works on an `inverseMany`.
- **Grid columns no longer send default values.** The protected field
  `SmartClientDataGridFieldDefinition.defaultValueJavascriptExpression` is gone.
- **Action `link`s** send the action's class name, ask the action's `confirm` text, and
  validate the form first unless the action sets `clientValidation="false"`.
- **PrimeFaces:** the `disabled` condition of a grid row's action link is now evaluated
  against the row. See [Inline Conditions](inline-conditions.md).

### The `external` Layout Family Is Gone

- A `UxUi` that names `external`, or names no PrimeFaces template, now uses `editorial`.
- A project's copy of `WEB-INF/pages/templates/external/` is now ignored. If you didn't
  customise it, delete it.
- If you did customise it, rename the folder, for example to `templates/myproject/`, and
  name it in your `UxUi`s:
  `UxUi.newPrimeFaces("external", "myproject", SKYVE_THEME, THEME_COLOUR)`.
  Then merge in later changes to `editorial/template.xhtml` and `view.xhtml`.
- A renamed copy is not treated as the Skyve family, so name a theme colour. Without one it
  gets `indigo`, and `dark` does not force dark mode.

## Known Issues

- **SmartClient: fast typing into a field whose change handler calls the server loses
  keys.**
  - Affected: a text field, password, text area, spinner or colour picker with a `rerender`
    or `server` action in `onChangedHandlers`. It keeps only the first key of a fast burst,
    such as a barcode scanner.
  - Typing at human speed is fine.
  - A fix was reverted before release. It will come with a rework of change and blur
    handling.
  - Until then, prefer `onBlurHandlers`. See SC-8 in [SAIL](sail.md).

## Known Security Notes

- **REST write routes check no permissions.**
  - `/rest/*` is forbidden by default.
  - If you enable it, the insert, update and delete routes save what the signed-in caller
    sends, unchecked. For example, a user could add roles to their own `admin.User`.
  - Enable it only for callers you trust with the whole data store, or add permission
    checks to `RestService` first.
  - Known; not fixed (decision of 2026-11-19).
