package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

@SuppressWarnings("static-method")
class JoinTableTest {

	@Test
	void constructorSetsIdentifiers() {
		JoinTable jt = new JoinTable("agnostic", "persistent", "ownerAgnostic", "ownerPersistent", false);
		assertEquals("agnostic", jt.agnosticIdentifier);
		assertEquals("persistent", jt.persistentIdentifier);
		assertEquals("ownerAgnostic", jt.ownerAgnosticIdentifier);
		assertEquals("ownerPersistent", jt.ownerPersistentIdentifier);
	}

	@Test
	void constructorSetsOrdered() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", true);
		assertTrue(jt.ordered);
	}

	@Test
	void constructorAddsOwnerAndElementFields() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", false);
		assertNotNull(jt.fields.get(PersistentBean.OWNER_COLUMN_NAME));
		assertNotNull(jt.fields.get(PersistentBean.ELEMENT_COLUMN_NAME));
	}

	@Test
	void constructorWithOrderedAddsOrdinalField() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", true);
		assertNotNull(jt.fields.get(Bean.ORDINAL_NAME));
	}

	@Test
	void constructorWithoutOrderedDoesNotAddOrdinalField() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", false);
		// ordinal field only present when ordered=true
		assertEquals(2, jt.fields.size());
	}
}
