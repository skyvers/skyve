/**
 * Database backup and restore framework for Skyve applications.
 *
 * <p>This package implements a full backup-and-restore cycle for Skyve
 * application data, including relational table data and binary content files:
 * <ul>
 *   <li>{@code BackupJob} — a {@link org.skyve.job.CancellableJob} that streams all
 *       customer tables and content repository files into a versioned ZIP archive.
 *   <li>{@code RestoreJob} — replays a backup ZIP to restore table data and content,
 *       applying the schema-migration options in {@code RestoreOptions}.
 *   <li>{@code DDL} — utility for executing DROP/CREATE SQL scripts against the
 *       database as part of a restore.
 *   <li>{@code Table} / {@code JoinTable} — metadata descriptors for tables
 *       and their fields inferred from the live domain model at backup time.
 *   <li>{@code BackupField} / {@code BackupLengthField} — column metadata holders.
 *   <li>{@code ContentChecker} — validates content attachments in the content store.
 *   <li>{@code ReindexJob} / {@code ReindexBeansJob} / {@code ReindexAttachmentsJob}
 *       — jobs that rebuild the content search index after a restore.
 *   <li>{@code Truncate} — utility for emptying all tables before restore.
 *   <li>{@code ExternalBackup} / {@code AzureBlobStorageBackup} — abstraction and
 *       Azure implementation for off-site backup storage.
 * </ul>
 */
package org.skyve.impl.backup;
