package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.lang.reflect.Method;

import org.junit.jupiter.api.Test;

import util.AbstractH2TestDispose;

class SkyveContextListenerH2Test extends AbstractH2TestDispose {
	@Test
	@SuppressWarnings("static-method")
	void initialiseSchemaCommits() throws Exception {
		Method method = SkyveContextListener.class.getDeclaredMethod("initialiseSchema");
		method.setAccessible(true); // NOSONAR - the test intentionally invokes a private lifecycle step

		assertDoesNotThrow(() -> method.invoke(null));
	}
}
