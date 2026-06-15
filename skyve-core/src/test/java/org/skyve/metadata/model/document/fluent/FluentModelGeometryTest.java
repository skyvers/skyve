package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Geometry;

/**
 * Tests for the model-attribute {@link FluentGeometry} (in
 * {@code metadata.model.document.fluent}), which wraps the field
 * {@code Geometry} type for document attribute metadata — distinct from the
 * identically-named view widget in {@code metadata.view.fluent}.
 */
@SuppressWarnings("static-method")
class FluentModelGeometryTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentGeometry().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		Geometry g = new Geometry();
		FluentGeometry fg = new FluentGeometry(g);
		assertEquals(g, fg.get());
	}

	@Test
	void fromDelegatesBaseFieldProperties() {
		Geometry source = new Geometry();
		source.setName("geoAttr");
		FluentGeometry copy = new FluentGeometry().from(source);
		assertEquals("geoAttr", copy.get().getName());
	}
}
