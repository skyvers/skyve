package org.skyve.impl.persistence;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.skyve.domain.PersistentBean;

public class NoOpDynamicPersistenceTest {

	@SuppressWarnings("resource")
	private final NoOpDynamicPersistence noop = new NoOpDynamicPersistence();

	@Test
	public void postConstructDoesNotThrow() {
		noop.postConstruct(null);
		assertNotNull(noop);
	}

	@Test
	public void persistDoesNotThrow() {
		noop.persist(null);
		assertNotNull(noop);
	}

	@Test
	public void deleteDoesNotThrow() {
		noop.delete(null);
		assertNotNull(noop);
	}

	@Test
	public void populateByIdReturnsNull() {
		assertNull(noop.populate("any-biz-id"));
	}

	@Test
	public void populateBeanDoesNotThrow() {
		noop.populate((PersistentBean) null);
		assertNotNull(noop);
	}

	@Test
	public void evictAllCachedDoesNotThrow() {
		noop.evictAllCached();
		assertNotNull(noop);
	}

	@Test
	public void evictCachedDoesNotThrow() {
		noop.evictCached(null);
		assertNotNull(noop);
	}

	@Test
	public void cachedReturnsFalse() {
		assertFalse(noop.cached(null));
	}

	@Test
	public void beginDoesNotThrow() {
		noop.begin();
		assertNotNull(noop);
	}

	@Test
	public void rollbackDoesNotThrow() {
		noop.rollback();
		assertNotNull(noop);
	}

	@Test
	public void commitDoesNotThrow() {
		noop.commit();
		assertNotNull(noop);
	}

	@Test
	public void closeDoesNotThrow() {
		noop.close();
		assertNotNull(noop);
	}
}
