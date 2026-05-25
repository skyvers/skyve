package org.skyve.metadata;

/**
 * Metadata that supports hot-reload and tracks when it was last checked.
 *
 * <p>The repository periodically checks whether on-disk metadata has changed
 * and triggers a reload if necessary. {@link #getLastCheckedMillis()} records
 * when the check was last performed so the repository can throttle repeated
 * file-system polls.
 *
 * @see PersistentMetaData
 */
public interface ReloadableMetaData {
	/**
	 * Used to throttle the checking for reload of meta-data.
	 * Thousands of calls to a persistent store can slow down performance of Skyve's dev mode.
	 * This value is used to throttle the modified checks in the repository.
	 * @return	The time the last check was made by the Skyve repository that loaded this metadata.
	 */
	public long getLastCheckedMillis();
	
	/**
	 * This is called by MutableCachedRepository for throttling the reload of meta-data in Skyve's dev mode.
	 * @param lastCheckedMillis	The time the last check was made by the Skyve repository that loaded this metadata.
	 */
	public void setLastCheckedMillis(long lastCheckedMillis);
}
