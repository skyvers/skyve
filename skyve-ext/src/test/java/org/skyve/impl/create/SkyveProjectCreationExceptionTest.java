package org.skyve.impl.create;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SkyveProjectCreationExceptionTest {

	@Test
	public void messageConstructorStoresMessage() {
		SkyveProjectCreationException ex = new SkyveProjectCreationException("test message");
		assertEquals("test message", ex.getMessage());
	}

	@Test
	public void messageCauseConstructorStoresBoth() {
		Throwable cause = new RuntimeException("cause");
		SkyveProjectCreationException ex = new SkyveProjectCreationException("msg", cause);
		assertEquals("msg", ex.getMessage());
		assertSame(cause, ex.getCause());
	}

	@Test
	public void isInstanceOfException() {
		SkyveProjectCreationException ex = new SkyveProjectCreationException("x");
		assertNotNull(ex);
	}
}
