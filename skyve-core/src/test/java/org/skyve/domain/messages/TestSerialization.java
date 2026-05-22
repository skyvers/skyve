package org.skyve.domain.messages;

import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

public class TestSerialization {
	@Test
	@SuppressWarnings("static-method")
	public void testValidationException() {
		assertNotNull(UtilImpl.cloneBySerialization(new ValidationException(new Message("testBinding", "testText"))));
	}
}
