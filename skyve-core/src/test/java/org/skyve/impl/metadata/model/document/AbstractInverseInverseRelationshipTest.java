package org.skyve.impl.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;

class AbstractInverseInverseRelationshipTest {

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsThreeRelationships() {
		assertEquals(3, InverseRelationship.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfOneToOne() {
		assertNotNull(InverseRelationship.valueOf("oneToOne"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfOneToMany() {
		assertNotNull(InverseRelationship.valueOf("oneToMany"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfManyToMany() {
		assertNotNull(InverseRelationship.valueOf("manyToMany"));
	}
}
