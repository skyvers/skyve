package org.skyve.archive.support;

import org.skyve.domain.Bean;
import org.skyve.domain.types.Timestamp;

public interface ArchiveableBean extends Bean {

    public static final String archiveTimestampPropertyName = "archiveTimestamp";

    public Timestamp getArchiveTimestamp();

    public String getArchiveFilename();

    public void setArchiveTimestamp(Timestamp archiveTimestamp);

    public void setArchiveFilename(String archiveFilename);
}
