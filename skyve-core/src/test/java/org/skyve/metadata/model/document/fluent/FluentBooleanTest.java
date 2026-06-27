package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link FluentBoolean}: constructors and {@code from()} delegation.
 * Note: uses {@code org.skyve.impl.metadata.model.document.field.Boolean}
 * (not {@code java.lang.Boolean}).
 */
@SuppressWarnings("static-method")
class FluentBooleanTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentBoolean().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.model.document.field.Boolean b =
				new org.skyve.impl.metadata.model.document.field.Boolean();
		FluentBoolean fb = new FluentBoolean(b);
		assertEquals(b, fb.get());
	}

	@Test
	void fromDelegatesAndReturnsThis() {
		org.skyve.impl.metadata.model.document.field.Boolean source =
				new org.skyve.impl.metadata.model.document.field.Boolean();
		source.setName("boolField");
		FluentBoolean copy = new FluentBoolean().from(source);
		assertEquals("boolField", copy.get().getName());
	}
}
