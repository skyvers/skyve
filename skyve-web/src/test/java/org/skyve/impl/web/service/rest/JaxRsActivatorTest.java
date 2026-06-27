package org.skyve.impl.web.service.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.content.rest.RestRemoteContentManagerServer;

class JaxRsActivatorTest {
	@Test
	@SuppressWarnings("static-method")
	void getClassesRegistersExpectedRestResources() {
		JaxRsActivator activator = new JaxRsActivator();

		Set<Class<?>> classes = activator.getClasses();

		assertEquals(2, classes.size());
		assertTrue(classes.contains(RestService.class));
		assertTrue(classes.contains(RestRemoteContentManagerServer.class));
	}
}