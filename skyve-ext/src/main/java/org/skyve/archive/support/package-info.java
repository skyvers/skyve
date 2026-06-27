/**
 * Public SPI contracts for the Skyve archive subsystem.
 *
 * <p>This package defines the interfaces that domain documents must implement (or
 * extend) to participate in the Skyve archiving lifecycle, and the converter abstraction
 * used by full-text search indexers:
 * <ul>
 *   <li>{@link org.skyve.archive.support.ArchiveableBean} — a {@link org.skyve.domain.Bean}
 *       extension that a document implements to indicate it can be archived. Archiveable
 *       beans carry an archive timestamp and an archive filename used by the archive job
 *       to record when and where the document was archived.
 *   <li>{@link org.skyve.archive.support.DocumentConverter} — converts a bean to a Lucene
 *       {@link org.apache.lucene.document.Document} for full-text indexing; used by
 *       content-search implementations that index archiveable documents.
 *   <li>{@link org.skyve.archive.support.CorruptArchiveError} — domain interface for the
 *       {@code CorruptArchiveError} admin document, which records archive entries that
 *       could not be processed cleanly.
 * </ul>
 *
 * <p>Concrete archive jobs and list models live in
 * {@code org.skyve.impl.archive} (skyve-ext).
 *
 * @see org.skyve.archive.support.ArchiveableBean
 */
package org.skyve.archive.support;
