package org.skyve.impl.create;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SkyveProjectCreationExceptionTest {

	@Test
	void messageConstructorStoresMessage() {
		SkyveProjectCreationException ex = new SkyveProjectCreationException("test message");
		assertEquals("test message", ex.getMessage());
	}

	@Test
	void messageCauseConstructorStoresBoth() {
		Throwable cause = new RuntimeException("cause");
		SkyveProjectCreationException ex = new SkyveProjectCreationException("msg", cause);
		assertEquals("msg", ex.getMessage());
		assertSame(cause, ex.getCause());
	}

	@Test
	void isInstanceOfException() {
		SkyveProjectCreationException ex = new SkyveProjectCreationException("x");
		assertNotNull(ex);
	}
}
