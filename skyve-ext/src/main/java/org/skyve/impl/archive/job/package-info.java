/**
 * Background jobs for the Skyve document archiving system.
 *
 * <p>This package provides the Quartz job implementations that drive archive
 * operations:
 * <ul>
 *   <li>{@code ArchiveJob} — the base archive job that orchestrates the archiving
 *       lifecycle.
 *   <li>{@code ExportDocumentsToArchiveJob} — exports document records matching an
 *       archive definition into the archive store.
 *   <li>{@code IndexArchivesJob} — (re-)indexes archived documents in Lucene so they
 *       are searchable via the archive list view.
 *   <li>{@code RecoverArchiveJob} — restores a previously archived document back into
 *       the live database.
 * </ul>
 *
 * @see org.skyve.impl.archive.support
 */
package org.skyve.impl.archive.job;
