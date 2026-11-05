package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.PersistentBean;

class NoOpDynamicPersistenceTest {

	@SuppressWarnings("resource")
	private final NoOpDynamicPersistence noop = new NoOpDynamicPersistence();

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void postConstructDoesNotThrow() {
		noop.postConstruct(null);
		assertNotNull(noop);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void persistDoesNotThrow() {
		noop.persist(null);
		assertNotNull(noop);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void deleteDoesNotThrow() {
		noop.delete(null);
		assertNotNull(noop);
	}

	@Test
	void populateByIdReturnsNull() {
		assertNull(noop.populate("any-biz-id"));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void populateBeanDoesNotThrow() {
		noop.populate((PersistentBean) null);
		assertNotNull(noop);
	}

	@Test
	void evictAllCachedDoesNotThrow() {
		noop.evictAllCached();
		assertNotNull(noop);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void evictCachedDoesNotThrow() {
		noop.evictCached(null);
		assertNotNull(noop);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void cachedReturnsFalse() {
		assertFalse(noop.cached(null));
	}

	@Test
	void beginDoesNotThrow() {
		noop.begin();
		assertNotNull(noop);
	}

	@Test
	void rollbackDoesNotThrow() {
		noop.rollback();
		assertNotNull(noop);
	}

	@Test
	void commitDoesNotThrow() {
		noop.commit();
		assertNotNull(noop);
	}

	@Test
	void closeDoesNotThrow() {
		noop.close();
		assertNotNull(noop);
	}
}
