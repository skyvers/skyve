package org.skyve.domain.messages;

import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

public class TestSerialization {
	@Test
	@SuppressWarnings("static-method")
	public void testValidationException() {
		UtilImpl.cloneBySerialization(new ValidationException(new Message("testBinding", "testText")));
	}
}
