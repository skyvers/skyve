package org.skyve.cache;

import java.io.Serializable;

import org.skyve.domain.Bean;

public class ArchivedDocumentCacheConfig extends EHCacheConfig<Serializable, Bean> {

    public static final String CACHE_NAME = "archivedDocuments";

    public static final ArchivedDocumentCacheConfig DEFAULT = new ArchivedDocumentCacheConfig(100l, 10l);

    public ArchivedDocumentCacheConfig(long heapSizeEntries, long expiryInMinutes) {

        super(CACHE_NAME, heapSizeEntries, 0l, CacheExpiryPolicy.timeToIdle, expiryInMinutes, Serializable.class, Bean.class);
    }

}
