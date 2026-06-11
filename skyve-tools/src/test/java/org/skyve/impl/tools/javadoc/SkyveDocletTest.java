package org.skyve.impl.tools.javadoc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.umlgraph.doclet.AccessibleOptions;

class SkyveDocletTest {

	@Test
	static void optionLengthAlwaysReturnsZero() {
		assertEquals(0, SkyveDoclet.optionLength("-any-option"));
		assertEquals(0, SkyveDoclet.optionLength(null));
	}

	@Test
	static void placeholderTypesRemainInstantiable() {
		assertNotNull(new SkyveDoclet());
		assertNotNull(new SkyveContextView());
		assertNotNull(new SkyvePackageView());
		assertNotNull(new AccessibleOptions());
	}
}
