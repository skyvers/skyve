package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class GeometryMapTest {

	@Test
	void showsLabelByDefaultReturnsTrue() {
		GeometryMap widget = new GeometryMap();
		assertTrue(widget.showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		GeometryMap widget = new GeometryMap();
		assertNotNull(widget.getProperties());
	}
}
