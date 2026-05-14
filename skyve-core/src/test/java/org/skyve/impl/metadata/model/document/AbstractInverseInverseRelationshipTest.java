package org.skyve.impl.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AbstractInverse.InverseRelationship;

public class AbstractInverseInverseRelationshipTest {

	@Test
	@SuppressWarnings("static-method")
	public void valuesContainsThreeRelationships() {
		assertEquals(3, InverseRelationship.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfOneToOne() {
		assertNotNull(InverseRelationship.valueOf("oneToOne"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfOneToMany() {
		assertNotNull(InverseRelationship.valueOf("oneToMany"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void valueOfManyToMany() {
		assertNotNull(InverseRelationship.valueOf("manyToMany"));
	}
}
