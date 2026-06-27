package org.skyve.domain.messages;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ValidationExceptionTest {

	@Test
	@SuppressWarnings("static-method")
	void getMessageWithMessagesIncludesMessageText() {
		ValidationException e = new ValidationException("myBinding", "Something went wrong");
		String msg = e.getMessage();
		assertNotNull(msg);
		assertTrue(msg.contains("Something went wrong"));
	}
}
