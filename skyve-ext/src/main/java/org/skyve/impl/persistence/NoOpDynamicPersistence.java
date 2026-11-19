package org.skyve.impl.persistence;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * No-operation {@link org.skyve.persistence.DynamicPersistence} implementation
 * that silently accepts all dynamic entity operations without persisting them.
 *
 * <p>Used in environments or tests where dynamic persistence is not required.
 */
public class NoOpDynamicPersistence implements DynamicPersistence {
	private static final long serialVersionUID = 2810341864186864562L;

	@Override
	public void postConstruct(@Nonnull Persistence persistence) {
		// No-op
	}
	
	@Override
	public void persist(@Nonnull PersistentBean bean) {
		// No-op
	}

	@Override
	public void delete(@Nonnull PersistentBean bean) {
		// No-op
	}

	@Override
	public @Nullable DynamicPersistentBean populate(@Nonnull String bizId) {
		// No-op
		return null;
	}

	@Override
	public void populate(@Nonnull PersistentBean bean) {
		// No-op
	}
	
	@Override
	public void evictAllCached() {
		// No-op
	}
	
	@Override
	public void evictCached(@Nonnull Bean bean) {
		// No-op
	}
	
	@Override
	public boolean cached(@Nonnull Bean bean) {
		// No-op
		return false;
	}

	@Override
	public void begin() {
		// No-op
	}

	@Override
	public void rollback() {
		// No-op
	}

	@Override
	public void commit() {
		// No-op
	}

	@Override
	public void close() {
		// No-op
	}
}
