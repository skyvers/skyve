package org.skyve.impl.web.faces.pipeline.component;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SkyveComponentBuilderChainTest {
	@Test
	void constructorRequiresFacesContextInHeadlessUnitScope() {
		assertThrows(NullPointerException.class, SkyveComponentBuilderChain::new);
	}
}
