package org.skyve.cache;

/**
 * Identifies the storage tier backing a cache entry.
 *
 * <p>The tier indicates where data is physically stored and the expected
 * trade-off between access latency and capacity.
 */
public enum CacheTier {
	/**
	 * Entry resides in JVM heap memory.
	 */
	OnHeap,

	/**
	 * Entry resides in off-heap/native memory.
	 */
	OffHeap,

	/**
	 * Entry is persisted to disk storage.
	 */
	Disk
}
