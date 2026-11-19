package org.skyve.impl.web.service.sse;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SseApplicationTest {

	@Test
	void getClassesReturnsSseClientHandlerClass() {
		SseApplication app = new SseApplication();
		Set<Class<?>> classes = app.getClasses();
		assertNotNull(classes);
		assertEquals(1, classes.size());
		assertTrue(classes.contains(SseClientHandler.class));
	}
}
