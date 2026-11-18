# Skyve 10 Changes

These notes collect release-ready framework changes introduced for Skyve 10.

- [Model-backed Lookup Descriptions](#model-backed-lookup-descriptions)
- [Managed Content Widget](content-widget.md)
- [Reusable Deployed Integration Testing](deployed-integration-testing.md)
- [Theme Resolution](theme-resolution.md)
- [View Boilerplate Escaping](view-boilerplate-escaping.md)
- [Inline Conditions in Views](inline-conditions.md)
  - Behaviour change: in PrimeFaces, the `disabled` condition of a `link` to an action
    inside a `dataGrid` or `dataRepeater` is now evaluated against the row, as its
    `invisible` condition already was. It was evaluated against the view's bean.
  - Behaviour change: `Bean.evaluateCondition("notX")` is now true when document condition
    `x` has a Skyve expression that yields `null`, as the generated `isNotX()` is.
    It was false, as `x` is, so neither sense was true.
- [Lucene Content Identifiers and Garbage Collection](#lucene-content-identifiers-and-garbage-collection)
- [Scheduled Backups and Azure Copies](#scheduled-backups-and-azure-copies)
- [Custom Property Order and Fluent Builders](#custom-property-order-and-fluent-builders)
- [Mail Log Body Excerpts and Attachments](#mail-log-body-excerpts-and-attachments)
- Decimal10 converters `Decimal10Integer`, `Decimal10IntegerPercentage`, `Decimal10OneDecimalPlace` and `Decimal10TwoDecimalPlacesPercentage` are new, and `Decimal10DollarsAndCents` can now be named in metadata.
- [Server-side Questions](#server-side-questions): server code can ask the user an OK/Cancel or Yes/No question with `EXT.getDialogs()`, in SmartClient and PrimeFaces.
- [Server-side Action Availability](server-side-action-availability.md): the server refuses a request for a view action that the view, as last rendered, offers the user no way to trigger.
- SmartClient `link`s to an action now send the action's class name, as its buttons do, so a link whose action is named apart from its class is no longer refused. They also ask the action's `confirm` text first.
  - Behaviour change: such a link now validates the form before it runs, as a button does, unless its action sets `clientValidation="false"`. It never validated before.
- The Editorial menu can be collapsed on wide screens with the menu button in the topbar, and stays collapsed on later pages. The setting is kept in the browser's local storage.
- A long page title in the Editorial topbar now stays at two lines, ending in an ellipsis. On a phone it could grow past the topbar and lie invisibly over the top of the action bar, so taps and swipes there hit the title instead of the buttons.
- The `external` PrimeFaces layout family has been removed. It was a copy of `editorial`. A `UxUi` that names `external`, or names no PrimeFaces template, now uses `editorial`.
  - Behaviour change: a project's own copy of `WEB-INF/pages/templates/external/` is now ignored, because the name `external` always resolves to `editorial`. If you didn't customise it, you can delete it; upgrading doesn't remove it. If you customised it and still need it, plumb it back in as a copy of `editorial` under a new name. Rename the folder, for example to `WEB-INF/pages/templates/myproject/`, and name it as the template in your `UxUis`, for example `UxUi.newPrimeFaces("external", "myproject", SKYVE_THEME, THEME_COLOUR)`. Merge in later changes to `editorial/template.xhtml` and `editorial/view.xhtml` as you would for any copy, such as the `showMenu` handling, the collapsed-menu script and the Vue bundle path below.
  - A renamed copy is not treated as the Skyve family, which only the name `editorial` is. Name a theme colour in the `UxUi`: with none, a copy gets `indigo` rather than `blue`. The `dark` colour does not force dark mode in a copy, as it does in `editorial`.
- In SmartClient, the New button of an inline `dataGrid` now asks the server for the new row, as PrimeFaces does. The form's values are posted, and the server creates the element, so its default values, its bizlet's `newInstance()`, the interceptors and its bizlet's `preExecute(ImplicitActionName.Add)` all run, and the row has a bizId. The view is then marked as changed. New also works on a grid bound to an `inverseMany`.
  - Behaviour change: these methods never ran for an inline SmartClient New before. An interceptor that vetoes the add now stops the row being added. A row being edited that fails validation is kept, and no row is added.
  - Behaviour change: SmartClient data grid columns no longer send their default value to the browser. The protected field `SmartClientDataGridFieldDefinition.defaultValueJavascriptExpression` has been removed, so a subclass that used it no longer compiles.
- Thumbnail and dynamic image requests are capped by the new optional `thumbnail.maximumDimension` setting in the JSON configuration, which defaults to `2048`. A bigger thumbnail or dynamic image is shrunk to fit, keeping its shape.
  - Behaviour change: content and resource requests make a thumbnail that fits a square of the larger requested side rounded up to 64, 128, 256 and so on, up to `thumbnail.maximumDimension`. So a request can only choose from a few sizes, and only a few are stored per image. The thumbnail is never smaller than the one asked for. An `<img/>` with a CSS width and height, and `object-fit:contain`, looks the same; one without them can show a bigger image than before.
  - `Util.getContentImageUrl()` now writes `width`, `height` and `object-fit:contain` on its `<img/>`, so it still shows at the size asked for.
  - Content image previews in forms and data grids ask for these sizes from the size of their box. A SmartClient data grid image column without a pixel width uses its column's width. SmartClient also allows for the screen's pixel ratio, so previews are sharp on high resolution screens.
  - A thumbnail is decoded at between once and twice its own size, so it is sharper than before but uses little memory: at most 16MB for a thumbnail of up to 1024 pixels, and about 64MB for one of 2048, twice that for a photo taken on its side. Lower `thumbnail.maximumDimension` to use less.
  - Behaviour change: an image of more than 256 megapixels is not read at all. It shows its file type icon, as a file that isn't an image does.
  - Behaviour change: `thumbnail.concurrentThreads` now defaults to `0`, which means half the processors. Either way it is limited to what a quarter of the maximum heap allows, at up to 128MB per thumbnail for the default `thumbnail.maximumDimension` of 2048, as a photo taken on its side is turned upright in a copy. So on a 512MB heap one thumbnail is made at once, and on a 4GB heap up to eight. Other requests wait their turn, and stored thumbnails are served without waiting. A request that cannot start within 60 seconds, a safety limit, gets a 503 (Service Unavailable) response with a `Retry-After` header. Before, it waited without limit.
  - Behaviour change: a thumbnail is no longer enlarged past the image's own size. An `<img/>` that shows a thumbnail without a CSS width and height now shows a small image at its own size. Size it with CSS, and use `object-fit:contain` to keep its shape.
  - Behaviour change: a content or resource request with only `_w` or only `_h` now returns a thumbnail sized by that side, not the original file. For a file that isn't an image, that is its file-type icon.
  - Behaviour change: a content request with `_nm` (no markup) always returns the original, whatever `_w` and `_h` ask for.
  - Behaviour change: content responses now let the user's browser keep them for a day (`Cache-Control: private, max-age=86400`), instead of a minute. Changing an attachment in Skyve gives it a new content id and so a new URL. Code that puts new bytes into an existing content id can be shown the old image for up to a day.
  - File-type icons for files that aren't images now keep their own shape within the requested size, so they are no longer drawn smaller in tall, narrow boxes.
  - Stored thumbnails are written under a temporary name and then renamed, so a crash can't leave a broken one. Nothing is stored when the thumbnail drive has less than 10% of its space free. Thumbnails that aren't stored are still made and served.
  - Removing an attachment now deletes its stored thumbnails too, however it is removed. With a remote content server (JDBC, REST or EJB), the client deletes them, as that is where they are made. Any code that removes attachments without a `ContentManager` should call `Thumbnail.deleteStoredThumbnails()`.
  - Once a day, content garbage collection deletes stored thumbnails made more than 90 days ago, and the folders this leaves empty. This clears the thumbnails of images that are gone, including those removed before this change. A thumbnail still in use is made again the next time it is shown. The thumbnail store has no size limit; it only stops growing when its drive has less than 10% free.
  - Behaviour change: a stored thumbnail of a file, such as a module's image resource, is made again when the file has changed since, for example after a redeploy. Before, the old thumbnail was served until the thumbnail folder was cleared.
  - Removed: `org.skyve.impl.web.filter.ThumbnailFilter`, added in 9.4.0, which Skyve itself never used. An app that maps it in `web.xml` fails to start until the mapping is removed. To serve thumbnails of an app's own files, extend `AbstractResourceServlet`, which checks access before serving a stored thumbnail.
  - Making thumbnails still takes CPU. Rate limit the `content` and `resources` URLs, and any app servlet that makes thumbnails, at your reverse proxy.
- Image shape fixes: SmartClient list grid thumbnail columns keep tall images inside the column's size; a CSS typo let them overflow. SmartClient data grid dynamic images take their width from their pixel width, not their pixel height. Contact images in the PrimeFaces templates and the SmartClient header now fill the round avatar, cropped, instead of being stretched.
- The thin environment banners, shown when `environment.identifier` is set, now also appear on the sign-in, error and other JSP pages, as well as on PrimeFaces and SmartClient views. The identifier is HTML-escaped. A blank identifier shows no banner.
- Number converters, list filters and date filters now agree across Java, SmartClient, the Vue list grid and PrimeFaces. See the Converters subsection of [SAIL](sail.md) for the rules.
  - Behaviour change: the number converters read the whole text, trimmed. Text after the number (`12abc`), an exponent (`1E3`), a `+` on a decimal, digits other than 0-9 and a `,` that is not between digits (`1,`, `,1`) are refused, where the start of the text was read before. A value that rounds to zero shows no sign (`0.00`, not `-0.00`).
  - Behaviour change: the number converters, `DecimalFormatter` and so the number formatters write and read English numbers (`1,234.50`) whatever the JVM's locale. A German JVM wrote `1.234,50` and read `1.5` as 15.
  - Behaviour change: a data file import (admin's Import Export, a tag's uploaded criteria and similar uploads) with a decimal converter still reads the number at the start of a cell (`1,234.50 kg` is 1234.50), and reads a spreadsheet's cells as English text.
  - Behaviour change: `Decimal5TimeDuration` and `TimeDurationFormatter` round the minutes once, half up, from the exact fraction of an hour (2.4999 hours is `2:30`, -0.01 is `-0:01`), and `-0:30` reads as -0.5.
  - Behaviour change: list filters' negations (not equal, not contains, not starts or ends with, not in, and Match None of a positive condition) now list the rows with no value in the column, as SmartClient's own evaluation does, in archived (Lucene) lists and, for lists built on a document query (the list grids on a document's or a metadata query), through an optional association. Match None negates a nested group as a whole, and not in an empty set lists every row.
  - Behaviour change: in a list built on a document query (also a list grid's chart), every filter condition through an association or an inverse one now left outer joins its path, where it was an implicit inner join that left out the rows with nothing on it. This covers positive conditions, negations, `isNull`, in and not in, ranges and geometry, every `nullOr` filter parameter operator, whether they come from the SmartClient or Vue list grids, a view's or a lookup's filter parameters, or code that filters a list model. Embedded associations are not joined, as their columns are on the row. The count matches the rows, and sorting on the association column keeps the rows with no association. A condition that asks for "no value" now lists the rows with nothing on that path: filtering `selection.description` with `isNull` lists the row with no selection, and `nullOrEqual` with `beta` lists the `beta` row and the row with no selection. Under Match Any, a row with no association that another criterion meets is kept: `selection.description` equals `beta` or `name` equals `AF n` lists the `beta` row and `AF n`, which has no selection, as SmartClient's own evaluation does. Under Match All, a positive condition through the association still leaves out the rows with no association. A view or lookup with a `nullOr` or `isNull` filter parameter on a dotted binding now lists more rows than before, and the SQL of every such list changes from an inner join to a left outer join, with rows differing only where Match Any, a negation or a "no value" check reaches rows with no association. A plain `notEqual` filter parameter still leaves out rows with no value; only `nullOrNotEqual` keeps them.
  - Bug fix: a view's or lookup's filter parameters with the `nullOrGreaterEqual`, `nullOrLess` and `nullOrLessEqual` operators all filtered with "greater than" since 2019. They now compare as named, so an app that relied on the old result lists different rows.
  - Known limitation: a query whose `from` declares several documents (a metadata query's `<from>` such as `{a.X} as bean, {a.Y} as y`) gets no joins from filters and keeps the inner joins of earlier releases, so there even a "no value" filter or a negation through an association leaves out the rows with no association. A custom `from` that inner joins the association itself, SQL based list models, and in-memory and archived lists are likewise unchanged.
  - Behaviour change: `between` is supported and, like `iBetween`, excludes both ends, as SmartClient defines them; `iBetween` included them. A range under Match None lists the rows outside it.
  - Behaviour change: in SmartClient, 12 hour times show `AM`/`PM` in upper case for every locale (`02:47 PM`), and a duration field refuses decimal hours without a colon (`2.5`).
  - Behaviour change: in SmartClient, a date time or timestamp whose converter shows only the date is filtered by the whole local day (also between days, and in exports, charts and Tag All), keeps its time when saved untyped, and a list that filters by such a column always fetches from the server.
  - Behaviour change: in SmartClient, the `Decimal2DollarsAndCentsAbsolute` column is filtered by signed dollars (`-7.50`), and the filter row shows an error for a number its converter refuses rather than dropping it.
  - Behaviour change: PrimeFaces date pickers read a typed `am` or `pm` in either case, and write 12 hour times as the converters do.
  - Behaviour change: the Vue list grid's date and time filter calendars take the column's converter format, a date-only date time column is filtered by the day, and a negative that rounds to zero shows `0.00`.

## Model-backed Lookup Descriptions

`lookupDescription` now accepts a `model` attribute to populate its choices from a
list model, in both SmartClient and PrimeFaces forms and data grids:

```xml
<lookupDescription binding="contact" descriptionBinding="name" model="ContactsModel" />
```

As with `listGrid`, the model belongs to the document owning the enclosing view,
including when the lookup is inside a data grid or uses a compound binding.
Customer overrides are supported. The model's driving document must match the
lookup's target document, and its columns must support the description and dropdown.

Specify either `query` or `model`, never both. Existing lookups need no changes:
omitting both still uses the association's query, then the target document's default
query. Model lookups generate `UserAccess.modelAggregate` access and require read
permission on the driving document. If view access generation is disabled, declare
the model access explicitly.

See [lookup model documentation](view_models.md#lookup-descriptions-backed-by-list-models)
for details, or try **Kitchen Sink → Model Lookup Fixture** using the
[SC/PF test steps](../skyve-war/src/main/java/modules/kitchensink/ModelLookupFixture/README.md).

## Lucene Content Identifiers and Garbage Collection

### What changed

Skyve's Lucene content manager now indexes an attachment's `contentId` as one exact,
stored value instead of analysed text. The field retains Lucene norms so the presence of
`contentId` can efficiently distinguish attachment records from `BeanContent` records:

- an attachment has a `contentId` field;
- `BeanContent` has no `contentId` field.

No separate content-kind field is added. This keeps the index compact while allowing
`FieldExistsQuery` to select attachments and its negation to select bean content.

The previous `TextField` mapping tokenised UUID content IDs at hyphens. Code that queried
with the complete UUID used an exact Lucene term, so an existing attachment could fail to
be found, updated or removed. A replacement could also be added without deleting the old
index document. The new exact field makes these operations use the same identifier
representation and ensures `IndexWriter.updateDocument()` replaces the current document
for that content ID.

This changes the Lucene schema only. It does not change content IDs stored in application
tables, attachment files, the `BeanContent` business-ID scheme or the Elastic content
manager.

### Affected behaviour

The change covers the following Lucene-backed operations:

- attachment lookup, update, reindex and removal by UUID content ID;
- prevention of duplicate current index documents during replacement or concurrent update;
- attachment-versus-bean classification during customer truncation and garbage collection;
- preservation of referenced attachments, including references in dynamic fields;
- removal of old orphaned attachments from both the index and `SKYVE_STORE`;
- removal of orphaned `BeanContent` left by direct delete or truncate SQL;
- the configured Quartz garbage-collection schedule and the manual Data Maintenance action.

Garbage collection continues to protect content with a missing modification timestamp,
content newer than the configured eligible age, content owned by persistent rows and
attachments referenced elsewhere. A failed lookup or removal is logged without preventing
other candidates from being processed, and collection remains bounded by its per-type
safety limit.

### Index compatibility

This is an intentional breaking Lucene index-schema change. A Lucene index written by an
earlier Skyve version must not be reused by Skyve 10. Deleting individual documents or
allowing new documents to accumulate alongside the legacy schema is not a supported
migration: Lucene field metadata remains associated with existing segments and the mixed
index can reject writes or retain legacy documents that exact-ID queries cannot address.

Do not run old and new Skyve nodes against the same content directory. Rebuild the complete
`SKYVE_CONTENT` index as part of the upgrade.

### Deployment when `content.fileStorage` is `true`

With filesystem content storage, `SKYVE_STORE` is the authoritative attachment store and
`SKYVE_CONTENT` is a disposable Lucene index. Use this upgrade procedure:

1. Take and verify backups of the database and the complete configured content directory.
   Confirm that attachment payload and metadata files exist under `SKYVE_STORE`.
2. Stop application traffic, all Skyve nodes and scheduled jobs that can write or garbage
   collect content. For a cluster, upgrade every node together.
3. Deploy Skyve 10 in maintenance mode using the same database and content directory.
4. From **Admin > Data Maintenance**, run **Drop Indexing** once for the shared content
   directory. This removes `SKYVE_CONTENT`; it must not remove `SKYVE_STORE`.
5. Run **Reindex** for every customer and wait for each job to complete. **Reindex** rebuilds
   both attachment and bean-content entries. Running only **Reindex Content** does not
   recreate `BeanContent` removed by the complete index drop.
6. Review the reindex logs for missing content, then verify representative attachment
   downloads, attachment lookup/search and bean-content search for every customer.
7. Re-enable the content garbage-collection schedule and normal application traffic only
   after verification succeeds.

The index may be unavailable, incomplete or slower for search while it is being rebuilt;
plan the maintenance window according to content volume. Keep the pre-upgrade backup until
the rebuilt index and garbage-collection run have both been verified.

### Deployment when `content.fileStorage` is `false`

When filesystem storage is disabled, attachment bytes are stored inside the Lucene index.
In this configuration, **Drop Indexing destroys attachment data** and the normal reindex job
cannot recover it after the drop.

Before upgrading, use the old Skyve version—while it can still read the legacy index—to
create and verify a Skyve backup with content included, or perform an application-specific
migration to filesystem-backed storage. Restore or migrate those attachments into a clean
Skyve 10 content store and build a new index there. Do not perform an in-place index drop,
and do not deploy Skyve 10 over the only copy of an inline-content index.

Exercise this migration against a production-sized copy first. Verify content counts and
representative bytes before retiring the legacy index.

## Scheduled Backups and Azure Copies

The scheduled backup job now makes each weekly, monthly and yearly copy once per period
instead of every day, the Azure external backup copies blobs server-side instead of
streaming them through the application server, and plain `YEARLY_` backups are now culled
to `yearlyBackupRetention` (previously they were never culled, so check that retention
before the first run after upgrading, and re-assemble the admin module to pick up the job
change).

## Custom Property Order and Fluent Builders

Custom properties (the `<properties>` section of any metadata element) are now kept in
declaration order instead of being sorted by key.

- `DecoratedMetaData.getProperties()` iterates in the order the properties were declared
  in XML or added through the fluent API. Replacing a property's value keeps its position.
- Marshalled metadata XML and the `MetaDataServlet` JSON list properties in that order.
- Fluent builders gain `putProperty(name, value)` wherever the metadata they build carries
  custom properties, and each builder's `from(...)` now copies the source's properties.
  For a module role this includes the properties on its document privileges, action
  privileges, content permissions and content restrictions.
- A `<property>` with no `key` is ignored when metadata is loaded.

## Mail Log Body Excerpts and Attachments

The Mail Log can now optionally store more of each outbound mail. Both settings are off by
default, live in the `smtp` stanza of the JSON configuration, and can be changed at runtime
from the Startup page, which writes them to the override configuration.

- `smtp.logBodyExcerpt`: when `true`, `MailLog.bodyExcerpt` stores the first 500 characters
  of the plain-text body instead of `[REDACTED]`. Only numeric codes that follow a known phrase
  ("verification code", "one-time code", "security code", "2FA code" and similar) are masked.
  Anything else in the body is stored as written, such as a temporary password or a plain-text
  reset link in a custom template, so check your templates before turning this on.
  `bodyExcerpt` is now 500 characters long (was 255); DDL sync widens the column.
- `smtp.logAttachments` and `smtp.logAttachmentsMaxBytes`: when `true`, the attachments sent
  are stored as a zip in the new `MailLog.attachments` content attribute, up to the given total
  size (default 5 MB). Attachments over the limit are left out and listed in `SKIPPED.txt`
  inside the zip; a negative limit is treated as 0. The zip is not text-indexed, but it is
  stored unmasked and can be downloaded by anyone who can read the Mail Log (by default the
  SecurityAdministrator role). It is kept until the Mail Log is hard
  deleted after archiving, when content garbage collection removes it. Archived Mail Logs
  do not keep their attachments: the content ID is cleared in the archived copy.
- `org.skyve.archive.support.DocumentConverter` gains a default no-op
  `prepareForArchive(Bean)`, called by the archive export job before each bean is written,
  so a document's converter can clear values that should not be archived.
- `org.skyve.domain.app.admin.MailLog` gains `getAttachments()` and `setAttachments(String)`.
  Re-assemble the admin module so its generated `MailLog` implements them.
## Server-side Questions

Server code can now ask the user a question part way through a request, where before
the only option was an action's static `confirm` text. Questions work from custom
actions, OK, Save, Delete, ZoomOut, rerenders and, in PrimeFaces, Remove, including
their Bizlet callbacks (`preSave`, `preDelete`, `preRerender` and so on) and interceptors:

```java
boolean charge = EXT.getDialogs().confirm(
		Question.yesNo("Invoice.lateFee", "Late fee", "Charge a late fee of " + fee + "?"));
EXT.getDialogs().ask(Question.okCancel("Invoice.post", "Post", "Post this invoice?").cancelAborts());
```

When a question has no answer yet, `ask` throws, the request is rolled back and the client
shows the question. The client then resends the same request with the answers so far, and
`ask` returns the chosen button. The code runs again from the start on each resend, so do
not ask after a commit or after a side effect outside the transaction, such as email,
push or content writes (see the `Dialogs.ask` Javadoc). Questions cannot be asked on form
load, while the response renders, from `postRender`, list grids, jobs or background tasks:
`canAsk()` is false there and `ask` throws `IllegalStateException`.

Try
**Kitchen Sink → Dialogs Fixture**.
