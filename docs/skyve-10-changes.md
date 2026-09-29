# Skyve 10 Changes

These notes collect release-ready framework changes introduced for Skyve 10.

- [Model-backed Lookup Descriptions](#model-backed-lookup-descriptions)
- [Managed Content Widget](content-widget.md)
- [Reusable Deployed Integration Testing](deployed-integration-testing.md)
- [Theme Resolution](theme-resolution.md)
- [View Boilerplate Escaping](view-boilerplate-escaping.md)
- [Lucene Content Identifiers and Garbage Collection](#lucene-content-identifiers-and-garbage-collection)
- [Scheduled Backups and Azure Copies](#scheduled-backups-and-azure-copies)

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
