package org.skyve.impl.persistence;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.skyve.domain.PersistentBean;

public class NoOpDynamicPersistenceTest {

	@SuppressWarnings("resource")
	private final NoOpDynamicPersistence noop = new NoOpDynamicPersistence();

	@Test
	public void postConstructDoesNotThrow() {
		noop.postConstruct(null);
	}

	@Test
	public void persistDoesNotThrow() {
		noop.persist(null);
	}

	@Test
	public void deleteDoesNotThrow() {
		noop.delete(null);
	}

	@Test
	public void populateByIdReturnsNull() {
		assertNull(noop.populate("any-biz-id"));
	}

	@Test
	public void populateBeanDoesNotThrow() {
		noop.populate((PersistentBean) null);
	}

	@Test
	public void evictAllCachedDoesNotThrow() {
		noop.evictAllCached();
	}

	@Test
	public void evictCachedDoesNotThrow() {
		noop.evictCached(null);
	}

	@Test
	public void cachedReturnsFalse() {
		assertFalse(noop.cached(null));
	}

	@Test
	public void beginDoesNotThrow() {
		noop.begin();
	}

	@Test
	public void rollbackDoesNotThrow() {
		noop.rollback();
	}

	@Test
	public void commitDoesNotThrow() {
		noop.commit();
	}

	@Test
	public void closeDoesNotThrow() {
		noop.close();
	}
}
