package org.skyve.impl.persistence;

import org.skyve.domain.Bean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.persistence.DynamicPersistence;
import org.skyve.persistence.Persistence;

public class NoOpDynamicPersistence implements DynamicPersistence {
	private static final long serialVersionUID = 2810341864186864562L;

	@Override
	public void postConstruct(Persistence persistence) {
		// No-op
	}
	
	@Override
	public void persist(PersistentBean bean) {
		// No-op
	}

	@Override
	public void delete(PersistentBean bean) {
		// No-op
	}

	@Override
	public DynamicPersistentBean populate(String bizId) {
		// No-op
		return null;
	}

	@Override
	public void populate(PersistentBean bean) {
		// No-op
	}
	
	@Override
	public void evictAllCached() {
		// No-op
	}
	
	@Override
	public void evictCached(Bean bean) {
		// No-op
	}
	
	@Override
	public boolean cached(Bean bean) {
		// No-op
		return false;
	}
}
