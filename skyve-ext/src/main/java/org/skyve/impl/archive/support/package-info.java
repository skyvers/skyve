/**
 * Low-level support utilities for the Skyve document archiving subsystem.
 *
 * <p>This package provides the building blocks used by the archive jobs and list model:
 * <ul>
 *   <li>{@code ArchiveLuceneIndexerSingleton} — manages per-document-type Lucene
 *       {@link org.apache.lucene.index.IndexWriter} instances; initialised on
 *       application startup and closed on shutdown.
 *   <li>{@code ArchiveRetriever} — reads individual archived document records from
 *       the flat-file archive store, using the Lucene index for positional lookups.
 *   <li>{@code ArchiveUtils} — static helpers for building archive file paths,
 *       generating string excerpts, and other cross-cutting utilities.
 *   <li>{@code FileLockRepo} — a singleton repository of per-file
 *       {@link java.util.concurrent.locks.ReentrantReadWriteLock} instances that
 *       serialises concurrent archive read and write operations.
 *   <li>{@code BufferedLineReader} — an efficient NIO-based line reader over an
 *       archive file, tracking byte offsets for positional random-access retrieval.
 * </ul>
 *
 * @see org.skyve.impl.archive.job
 * @see org.skyve.impl.archive.list
 */
package org.skyve.impl.archive.support;
