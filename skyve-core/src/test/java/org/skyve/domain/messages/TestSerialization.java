package org.skyve.domain.messages;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

class TestSerialization {
	@Test
	@SuppressWarnings("static-method")
	void testValidationException() {
		assertNotNull(UtilImpl.cloneBySerialization(new ValidationException(new Message("testBinding", "testText"))));
	}
}
