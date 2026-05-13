package org.skyve.impl.persistence.hibernate;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.mock;

import org.hibernate.persister.entity.EntityPersister;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link HibernateListener}.
 */
class HibernateListenerTest {

	/** requiresPostCommitHanding() always returns false — verifies the contract. */
	@Test
	@SuppressWarnings("static-method")
	void testRequiresPostCommitHandingReturnsFalse() {
		HibernateListener listener = new HibernateListener();
		EntityPersister persister = mock(EntityPersister.class);
		assertFalse(listener.requiresPostCommitHanding(persister));
	}
}
