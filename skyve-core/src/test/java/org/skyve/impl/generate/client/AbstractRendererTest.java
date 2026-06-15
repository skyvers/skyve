package org.skyve.impl.generate.client;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.web.UserAgentType;

class AbstractRendererTest {
	@Test
	@SuppressWarnings("static-method")
	void setUserAgentTypeUpdatesProtectedField() {
		TestRenderer renderer = new TestRenderer();

		renderer.setUserAgentType(UserAgentType.desktop);

		assertEquals(UserAgentType.desktop, renderer.userAgentType);
	}

	private static final class TestRenderer extends AbstractRenderer {
		// no additional behavior required
	}
}