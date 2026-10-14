package org.skyve.impl.cache;

import org.ehcache.Cache;
import org.ehcache.CacheManager;
import org.ehcache.CachePersistenceException;
import org.ehcache.PersistentCacheManager;
import org.ehcache.StateTransitionException;
import org.ehcache.Status;
import org.ehcache.config.Builder;
import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.Configuration;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A wrapper for a simple non-persistent, on-heap EHCache Manager that ensures no cache folder (and lock file) is created
 * and no off-heap memory is required.
 */
public class NonPersistentCacheManager implements PersistentCacheManager {
	private CacheManager delegate;
	
	public NonPersistentCacheManager(@Nonnull CacheManager delegate) {
		this.delegate = delegate;
	}
	
	@Override
	public @Nonnull <K, V> Cache<K, V> createCache(@Nonnull String alias, @Nonnull CacheConfiguration<K, V> config) {
		return delegate.createCache(alias, config);
	}

	@Override
	public @Nonnull <K, V> Cache<K, V> createCache(@Nonnull String alias, @Nonnull Builder<? extends CacheConfiguration<K, V>> configBuilder) {
		return delegate.createCache(alias, configBuilder);
	}

	@Override
	public @Nullable <K, V> Cache<K, V> getCache(@Nonnull String alias, @Nonnull Class<K> keyType, @Nonnull Class<V> valueType) {
		return delegate.getCache(alias, keyType, valueType);
	}

	@Override
	public void removeCache(@Nonnull String alias) {
		delegate.removeCache(alias);
	}

	@Override
	public void init() throws StateTransitionException {
		delegate.init();
	}

	@Override
	public void close() throws StateTransitionException {
		delegate.close();
	}

	@Override
	public @Nonnull Status getStatus() {
		return delegate.getStatus();
	}

	@Override
	public @Nonnull Configuration getRuntimeConfiguration() {
		return delegate.getRuntimeConfiguration();
	}

	@Override
	public void destroy() throws CachePersistenceException {
		// no-op
	}

	@Override
	public void destroyCache(@Nonnull String alias) throws CachePersistenceException {
		// no-op
	}
}
