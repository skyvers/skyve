package org.skyve.impl.metadata.view.widget.bound.tabular;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ListRepeaterTest {

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertTrue(new ListRepeater().getProperties() != null);
	}
}
