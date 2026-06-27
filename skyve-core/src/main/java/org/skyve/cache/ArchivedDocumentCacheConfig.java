package org.skyve.cache;

import java.io.Serializable;

import org.skyve.domain.Bean;

/**
 * Configures cache settings for archived document payloads.
 *
 * <p>Entries are keyed by archived document identifier and store
 * {@link Bean} snapshots with time-to-idle expiry.
 */
public class ArchivedDocumentCacheConfig extends EHCacheConfig<Serializable, Bean> {
    /**
     * Cache region name used for archived document snapshots.
     */
    public static final String CACHE_NAME = "archivedDocuments";

    /**
     * Default archived document cache settings.
     */
    public static final ArchivedDocumentCacheConfig DEFAULT = new ArchivedDocumentCacheConfig(100l, 10l);

    /**
     * Creates archived document cache configuration.
     *
     * @param heapSizeEntries maximum heap entry count
     * @param expiryInMinutes time-to-idle duration in minutes
     */
    public ArchivedDocumentCacheConfig(long heapSizeEntries, long expiryInMinutes) {
        super(CACHE_NAME, heapSizeEntries, 0l, CacheExpiryPolicy.timeToIdle, expiryInMinutes, Serializable.class, Bean.class);
    }
}
