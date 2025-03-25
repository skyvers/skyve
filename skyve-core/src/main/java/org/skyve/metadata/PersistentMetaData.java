package org.skyve.metadata;

public interface PersistentMetaData extends SerializableMetaData {
	/**
	 * Used to determine if the meta-data persisted is newer than what was loaded in memory.
	 * This allows meta-data reload when Skyve is in dev mode.
	 * @return	The millis from the meta-data store - eg the file modified time for a file system meta-data repository
	 */
	public long getLastModifiedMillis();
}
