package org.skyve.cache;

import java.io.Serializable;

import org.skyve.domain.Bean;

public class ArchivedDocumentCacheConfig extends EHCacheConfig<Serializable, Bean> {

    public static final String CACHE_NAME = "archivedDocuments";

    public static final ArchivedDocumentCacheConfig DEFAULT = new ArchivedDocumentCacheConfig(
            100l, 0l, CacheExpiryPolicy.timeToIdle, 10l);

    public ArchivedDocumentCacheConfig(
            long heapSizeEntries,
            long offHeapSizeInMB,
            CacheExpiryPolicy expiryPolicy,
            long expiryInMinutes) {

        super(CACHE_NAME, heapSizeEntries, offHeapSizeInMB, expiryPolicy, expiryInMinutes, Serializable.class, Bean.class);
    }

}
