package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class GeometryTest {

	@Test
	@SuppressWarnings("static-method")
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new Geometry().showsLabelByDefault());
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertiesReturnsNonNull() {
		assertNotNull(new Geometry().getProperties());
	}
}
