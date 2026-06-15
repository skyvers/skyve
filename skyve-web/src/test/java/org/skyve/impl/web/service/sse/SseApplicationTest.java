package org.skyve.impl.web.service.sse;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SseApplicationTest {

	@Test
	public void getClassesReturnsSseClientHandlerClass() {
		SseApplication app = new SseApplication();
		Set<Class<?>> classes = app.getClasses();
		assertNotNull(classes);
		assertEquals(1, classes.size());
		assertTrue(classes.contains(SseClientHandler.class));
	}
}
