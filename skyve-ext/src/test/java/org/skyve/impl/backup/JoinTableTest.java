package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

@SuppressWarnings("static-method")
public class JoinTableTest {

	@Test
	public void constructorSetsIdentifiers() {
		JoinTable jt = new JoinTable("agnostic", "persistent", "ownerAgnostic", "ownerPersistent", false);
		assertEquals("agnostic", jt.agnosticIdentifier);
		assertEquals("persistent", jt.persistentIdentifier);
		assertEquals("ownerAgnostic", jt.ownerAgnosticIdentifier);
		assertEquals("ownerPersistent", jt.ownerPersistentIdentifier);
	}

	@Test
	public void constructorSetsOrdered() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", true);
		assertTrue(jt.ordered);
	}

	@Test
	public void constructorAddsOwnerAndElementFields() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", false);
		assertNotNull(jt.fields.get(PersistentBean.OWNER_COLUMN_NAME));
		assertNotNull(jt.fields.get(PersistentBean.ELEMENT_COLUMN_NAME));
	}

	@Test
	public void constructorWithOrderedAddsOrdinalField() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", true);
		assertNotNull(jt.fields.get(Bean.ORDINAL_NAME));
	}

	@Test
	public void constructorWithoutOrderedDoesNotAddOrdinalField() {
		JoinTable jt = new JoinTable("a", "p", "oa", "op", false);
		// ordinal field only present when ordered=true
		assertEquals(2, jt.fields.size());
	}
}
