package org.skyve.metadata;

/**
 * Metadata that records the timestamp of its last modification on disk.
 *
 * <p>The repository uses this timestamp to detect whether cached metadata is
 * stale and must be reloaded. A cached instance is considered stale when the
 * file system's last-modified time is more recent than
 * {@link #getLastModifiedMillis()}.
 *
 * @see ReloadableMetaData
 */
public interface PersistentMetaData extends SerializableMetaData {
	/**
	 * Used to determine if the meta-data persisted is newer than what was loaded in memory.
	 * This allows meta-data reload when Skyve is in dev mode.
	 * @return	The millis from the meta-data store - eg the file modified time for a file system meta-data repository
	 */
	public long getLastModifiedMillis();
}
