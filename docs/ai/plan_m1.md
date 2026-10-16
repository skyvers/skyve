# Plan: M1 - Optional MailLog attachment storage

## Summary
Let administrators opt in to storing the actual attachment files sent with each outbound mail against its `admin.MailLog` entry, instead of only the attachment file names. Storage is off by default, gated by a new `smtp.logAttachments` flag (configurable from Startup and persisted to the override JSON), and bounded by a total-size cap. Attachments are stored as a single zip in one new `content` attribute on `MailLog`, which avoids a child document and keeps the archive/purge path working unchanged.

Affected modules: `skyve-core` (config field, `MailLog` interface), `skyve-ext` (`MailLogUtil` persistence), `skyve-web` (config parsing), `skyve-war` (MailLog + Startup metadata, views, Startup extension, sample config). Depends on domain generation working again (see Dependencies).

## Scope
1. **Config flags** (locked: separate from `logBodyExcerpt`, because whole unmasked files carry a different risk and storage cost than a 500-character masked excerpt)
   - `smtp.logAttachments` (boolean, optional, default `false`) → `UtilImpl.SMTP_LOG_ATTACHMENTS`
   - `smtp.logAttachmentsMaxBytes` (integer, optional, default `5242880` = 5 MB) → `UtilImpl.SMTP_LOG_ATTACHMENTS_MAX_BYTES`. The cap applies to the total raw size of the attachments stored for one log entry.
2. **MailLog metadata**: new `content` attribute `attachments` ("Attachments"), plus the matching `getAttachments()/setAttachments(String)` on `org.skyve.domain.app.admin.MailLog`. Text indexing is turned off for this attribute so attachment contents never reach the full-text index (see Implementation 2).
3. **Capture**: when the flag is on, `MailLogUtil` zips the attachments of the logged mail(s) into `mail-attachments.zip` and stores it through the content manager against the log's `bizId`/`attachments` attribute.
   - Bulk: de-duplicate across mails by (file name, SHA-256 of bytes), so one PDF sent to 500 recipients is stored once. If the same name appears with different bytes, the later entries get a `name (2).ext` suffix.
   - Cap: add files in `attachmentFileNames` order until the next one would exceed the cap. Leave out the rest and list them in a `SKIPPED.txt` entry inside the zip (name + size + reason), so an admin can see why a file is missing.
   - Null attachments, attachments with no bytes, and attachments with a blank name are skipped. Nameless files get `attachment-{n}`.
4. **Startup**: add `mailLogAttachments` (checkbox) and `mailLogAttachmentsMaxBytes` (integer, min 0) to the mail settings, loading and saving `smtp.logAttachments` / `smtp.logAttachmentsMaxBytes` the same way as `logBodyExcerpt`.
5. **MailLog view**: show the `attachments` content link (read-only) under the attachment file names, visible only when there is content.
6. **Docs**: `docs/smtp.md` field table + security note; `MailLog.xml` description; sample `skyve.json` files.

Out of scope: per-file child documents, inline preview, restoring attachments from archive, attachments for inbound mail.

## Affected modules
- `skyve-core`: `UtilImpl` (two new static fields + reset), `org.skyve.domain.app.admin.MailLog` (content accessor pair). Contract change, see Backward compatibility.
- `skyve-ext`: `org.skyve.impl.mail.MailLogUtil` (zip building, size cap, content put in `persist`), `MailLogUtilTest`.
- `skyve-web`: `SkyveContextListener.configureMailServiceAndSmtp` (parse both keys, both optional), `SkyveContextListenerTest`.
- `skyve-war`: `admin/MailLog/MailLog.xml`, `admin/MailLog/views/edit.xml`, `admin/Startup/Startup.xml`, `Startup/views/edit.xml`, `StartupExtension` (+ test), `config/skyve.json`, regenerated `modules/admin/domain/MailLog.java`, `Startup.java`, `admin_orm.hbm.xml`.

## Dependencies
- **Blocking: `skyve:generateDomain` must work.** It currently fails on master with `Action privilege ConditionsExport for customer demo in module kitchensink for document ConditionsFixture for role dev does not reference a valid action` (introduced by `a9c43ac63`, "Add conditions fixture to kitchen sink"). Under ADR-002 the generateDomain validation gate is mandatory, and this milestone adds a new attribute to `MailLog`, so that error must be fixed first, as a separate change.
- Once generation works, regenerate and replace the hand-edited generated files from the `maillog-body-excerpt` branch (`Startup.java` `mailLogBodyExcerpt` accessors, `admin_orm.hbm.xml` `bodyExcerpt length="500"`). Check that the regenerated output matches.
- Builds on the `maillog-body-excerpt` branch (`smtp.logBodyExcerpt`, Startup wiring). Implement on top of it or after it merges.

## Backward compatibility
- Behaviour: off by default, so existing apps keep storing only file names. No config change is required.
- Schema: one new nullable `attachments` column on `ADM_MailLog`, added by DDL sync. No data migration.
- API: adding methods to the `org.skyve.domain.app.admin.MailLog` interface breaks any downstream class that implements it directly. In practice only the generated `modules.admin.domain.MailLog` implements it, and apps pick up a regenerated one with the new admin module. State this in `docs/skyve-10-changes.md`.
- Config: both keys are optional. Older `skyve.json` / override files without them still load.

## Current state observed
- `MailLogUtil.createEntry` collects attachment names only (`TreeSet` → `attachmentFileNames` memo). The `MailAttachment` bytes are in memory at that point and then dropped.
- `MailLogUtil.persist` writes in an isolated `AbstractHibernatePersistence` with `upsertBeanTuple(log)` and never fails the caller's mail flow (catch-all + rollback). A test `Recorder` short-circuits persistence for unit tests.
- Archiving (`ExportDocumentsToArchiveJob`): exports each row with `JSON.marshall` of a projected `DynamicBean`, soft-deletes it (`archiveTimestamp`/`archiveFilename`), then after `retainDeletedDocumentsDays` hard-deletes with raw SQL `delete from ADM_MailLog where bizId in :ids`. Because this skips Skyve delete, a child collection would hit FK violations here. A single content column has no FK.
- `ContentGarbageCollectionJob` removes content whose owning persistent row no longer exists (with an age guard). So a zip left behind by the raw hard delete, or by a failed log commit after the content put, gets cleaned up by GC with no new code.
- `AttachmentContent(customer, module, document, dataGroupId, userId, bizId, attributeName).attachment(fileName, MimeType.zip, bytes)` is the content API; `MimeType.zip` exists. `Communication` already uses `content` attributes in the admin module.
- `Field.IndexType` has `database, textual, both, none`.

## Implementation plan
1. **skyve-core**
   - `UtilImpl`: `public static boolean SMTP_LOG_ATTACHMENTS = false;` and `public static long SMTP_LOG_ATTACHMENTS_MAX_BYTES = 5242880L;` next to `SMTP_LOG_BODY_EXCERPT`, plus resets in the reset method.
   - `MailLog` interface: `String getAttachments(); void setAttachments(String attachments);` with Javadoc (content id of the zip, null when not stored).
2. **skyve-war metadata**
   - `MailLog.xml`: `<content name="attachments" required="false"><displayName>Attachments</displayName><index>none</index></content>` after `attachmentFileNames`. First check that `index none` on a `content` attribute stops textual extraction in the content manager. If it doesn't, record that as a risk and look at `AttachmentContent` for a non-indexing option.
   - Update the `MailLog.xml` description to mention optional attachment storage.
   - `MailLog/views/edit.xml`: read-only content link bound to `attachments`, shown only when `attachments` is not null.
   - `Startup.xml`: `mailLogAttachments` boolean (default false) and `mailLogAttachmentsMaxBytes` long/integer (default 5242880, `min="0"`) with clear descriptions. Add both to `Startup/views/edit.xml` under "Log Body Excerpt".
   - Run `mvn -B -DskipTests compile` then `mvn -B skyve:generateDomain`.
3. **skyve-web**: in `configureMailServiceAndSmtp`, read `logAttachments` (`Boolean.TRUE.equals(...)`) and `logAttachmentsMaxBytes` (`getNumber(..., false)`, falling back to the default when null), both optional.
4. **skyve-ext `MailLogUtil`**
   - Add a nullable `byte[] attachmentsZip` to `MailLogEntry` (+ getter) so the test recorder can assert on it.
   - `createEntry`: when `UtilImpl.SMTP_LOG_ATTACHMENTS`, call a new `static @Nullable byte[] zipAttachments(List<Mail> mails, long maxBytes)` (dedupe, cap, `SKIPPED.txt`, `ZipOutputStream` over a `ByteArrayOutputStream`). Return null when there's nothing to store.
   - `persist`: before `upsertBeanTuple`, if the zip is not null, generate the log's `bizId` (already set by `newInstance`), build `AttachmentContent(superUser.getCustomerName(), "admin", "MailLog", log.getBizDataGroupId(), log.getBizUserId(), log.getBizId(), MailLog.attachmentsPropertyName).attachment("mail-attachments.zip", MimeType.zip, zip)`, `put` it with `EXT.newContentManager()` in try-with-resources, and `log.setAttachments(content.getContentId())`. If storing the content fails, log a warning and still persist the log without attachments (best-effort, like the rest of MailLog).
   - Use the `AppConstants` module/document name constants, not string literals.
5. **StartupExtension**: `SMTP_LOG_ATTACHMENTS_KEY = "logAttachments"`, `SMTP_LOG_ATTACHMENTS_MAX_BYTES_KEY = "logAttachmentsMaxBytes"`. Load in the constructor/load block and save in `putMail` following the `logBodyExcerpt` pattern.
6. **Docs/config**: sample `skyve.json` entries (`"logAttachments": false`, `"logAttachmentsMaxBytes": 5242880`) in `skyve-war/config/skyve.json` and `skyve-core/src/test/resources/json/skyve.json`; `docs/smtp.md` table rows + a security note (attachments are stored unmasked and readable by any role with MailLog read access); `docs/skyve-10-changes.md` interface note.

## Acceptance checklist
- [x] Flag off (default): no zip is built, `attachments` is null, behaviour is unchanged.
- [x] Flag on, single mail with 2 attachments: zip has both files with the original names and bytes; `attachments` holds a content id that downloads from the MailLog view. (Checked in a local WildFly deploy: the zip downloads from the MailLog view.)
- [x] Bulk mail with the same attachment on every mail: zip holds it once.
- [x] Same name, different bytes: both are stored, the second with a `(2)` suffix.
- [x] Total over the cap: files that fit are stored; `SKIPPED.txt` lists the rest with sizes.
- [x] Mail with no attachments: no content is stored.
- [x] Content manager failure: MailLog row is still persisted without attachments; the mail send is unaffected.
- [x] Startup page loads and saves both settings, writes them to the override JSON only when changed, and updates `UtilImpl` at runtime.
- [x] `skyve:generateDomain` passes, and the regenerated domain matches the metadata (no hand edits left in `src/generated`).
- [x] Attachment contents are not full-text searchable. (Stored with `ContentManager.put(content, false)`, which is unit tested, and the attribute has `<index>none</index>`.)
- [ ] Archive export + hard delete of a MailLog with attachments succeeds, and content GC later removes the orphaned zip.

## Better engineering notes
- A single zip trades per-file preview for no child table, no FK in the raw-SQL purge, and one content object per log. If per-file access is wanted later, a `MailLogAttachment` child document needs the archive purge changed to delete children first.
- Archived (soft-deleted) logs keep their content id until hard delete. After that the archived JSON references content that GC has removed. Attachments are therefore kept only for the live and retention windows. Document this; restoring attachments from archive is out of scope.
- Follow-up: fix the kitchensink `ConditionsExport` privilege so `generateDomain` works again (prerequisite, separate change).
- Follow-up idea: a "Clear stored attachments" admin action, or a separate retention period shorter than the MailLog retention.

## AI validation plan
- `mvn -q -pl skyve-core,skyve-ext,skyve-web -am install -DskipTests`
- `cd skyve-war && mvn -B -DskipTests compile && mvn -B skyve:generateDomain`
- Unit tests: `MailLogUtilTest` (new tests for each zip case: off, on, dedupe, name clash, cap + `SKIPPED.txt`, empty, content failure via a mocked content manager), `SkyveContextListenerTest` (both keys, defaults when absent), `StartupExtensionTest`, `StartupDomainTest`, `LoggingMailServiceTest`.
- `mvn -B package` baseline before handoff.
- Local deploy (skyve-local-deploy): turn on in Startup, send a test mail with attachments via `testBogusSend`, download the zip from the MailLog view.

## AI validation results
- `skyve:generateDomain` passed (the kitchensink `ConditionsExport` error didn't come up). The regenerated `Startup.java` and `admin_orm.hbm.xml` matched the earlier hand edits; this run's diffs are purely additive.
- `MailLogUtilTest` (37), `LoggingMailServiceTest` (19), `SkyveContextListenerTest` (61): pass.
- skyve-war `StartupExtensionTest` (18), `StartupDomainTest` (26), `MailLog*Test` / `MailLogList*Test` (33), `ViewValidatorTest` (95): pass.
- Local WildFly deploy (Eclipse), with Test Mode on: turned on Log Attachments and Log Body Excerpt in Startup, and the override JSON got `logAttachments`/`logBodyExcerpt` (`logAttachmentsMaxBytes` was unchanged, so not written). A Communication Test Send with `report.txt` (59 bytes) and `invoice.pdf` (2048 bytes) created a SKIPPED MailLog. Its Attachments link served `mail-attachments.zip` with both files byte-for-byte, and the body excerpt showed the verification code as `******`. No related server errors.
- Not run yet: full `mvn -B package` (a local `package` run stopped on 383 `modules.test` errors: `target/classes/customers/demo/demo.xml` was missing from that build), and an archive + hard delete + content GC cycle.
- Implementation notes: attachments are added in mail order (not `attachmentFileNames` order). The Startup max-bytes setting is a `longInteger` with a spinner. `MailLog` interface methods follow the interface's existing style (no per-method Javadoc).

## User validation suggestions
- Turn on "Log Attachments" in Startup, send a mail with two attachments, and download the zip from the Mail Log.
- Send a bulk mail with a shared attachment and check the zip holds one copy.
- Set the cap to a small value and confirm `SKIPPED.txt` lists the large file.
- Check `skyve-overrides.json` holds the two new keys only after you change them.
- Search the Mail Log / global text search for a word that only appears inside an attachment and confirm it isn't found.
