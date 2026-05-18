package org.skyve.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.Inverse.InverseCardinality;

class InverseCardinalityTest {

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsTwoCardinalities() {
		assertEquals(2, InverseCardinality.values().length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfOne() {
		assertNotNull(InverseCardinality.valueOf("one"));
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfMany() {
		assertNotNull(InverseCardinality.valueOf("many"));
	}
}
