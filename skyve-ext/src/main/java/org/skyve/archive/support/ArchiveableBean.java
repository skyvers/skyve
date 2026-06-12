package org.skyve.archive.support;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;

/**
 * Marker interface for domain beans that participate in the Skyve archive lifecycle.
 *
 * <p>A document that implements {@code ArchiveableBean} can be processed by the archive
 * job, which writes expired records to an archive store and records metadata about the
 * archival on the bean itself. Two fields drive the archival record:
 * <ul>
 *   <li>{@link #getArchiveTimestamp()} / {@link #setArchiveTimestamp(Timestamp)} — the
 *       point in time when the bean was archived; {@code null} until the archive job
 *       processes the record.
 *   <li>{@link #getArchiveFilename()} / {@link #setArchiveFilename(String)} — the name
 *       of the archive file or storage key where this record's data was written.
 * </ul>
 *
 * <p>Property name constants ({@link #archiveTimestampPropertyName},
 * {@link #archiveFilenamePropertyName}) are provided for use in binding expressions and
 * metadata queries.
 *
 * @see org.skyve.archive.support
 */
public interface ArchiveableBean extends Bean {

    @SuppressWarnings("java:S115") // Property-name constants mirror generated domain binding names.
    public static final String archiveTimestampPropertyName = "archiveTimestamp";
    @SuppressWarnings("java:S115") // Property-name constants mirror generated domain binding names.
    public static final String archiveFilenamePropertyName = "archiveFilename";

    public Timestamp getArchiveTimestamp();

    public String getArchiveFilename();

    public void setArchiveTimestamp(Timestamp archiveTimestamp);

    public void setArchiveFilename(String archiveFilename);
}
