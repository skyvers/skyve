package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.web.WebErrorUtil;

class SmartClientEditServletErrorResponseTest {
	private static final String REFERENCE = "2f8f0e2c-3b93-4cc2-9d9d-5f24ec777a3d";

	@Test
	@SuppressWarnings("static-method")
	void technicalFetchErrorUsesGenericMessageAndFetchMetadata() {
		String response = produce(new IllegalStateException("Hibernate QueryException from table SECRET_TABLE"),
									Operation.fetch,
									false,
									REFERENCE);

		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains("\"startRow\":0,\"endRow\":0,\"totalRows\":0"));
		assertTrue(response.contains(WebErrorUtil.genericMessage(REFERENCE)));
		assertFalse(response.contains("Hibernate"));
		assertFalse(response.contains("SECRET_TABLE"));
	}

	@Test
	@SuppressWarnings("static-method")
	void technicalNonFetchErrorOmitsFetchMetadata() {
		String response = produce(new IllegalArgumentException("SQL select * from secret"),
									Operation.update,
									true,
									REFERENCE);

		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains(WebErrorUtil.genericMessage(REFERENCE)));
		assertFalse(response.contains("startRow"));
		assertFalse(response.contains("SQL select"));
	}

	@Test
	@SuppressWarnings("static-method")
	void boundMessageExceptionPreservesFieldErrors() {
		String response = produce(new TestMessageException(new Message("customer.name", "Name is required")),
									Operation.update,
									true,
									REFERENCE);

		assertTrue(response.contains("\"status\":-4"));
		assertTrue(response.contains("\"customer_name\":\"Name is required\""));
		assertFalse(response.contains(WebErrorUtil.GENERIC_ERROR_MESSAGE));
	}

	@Test
	@SuppressWarnings("static-method")
	void unboundMessageExceptionPreservesBusinessMessages() {
		String response = produce(new TestMessageException(new Message("Cannot approve this record")),
									Operation.fetch,
									true,
									REFERENCE);

		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains("\"startRow\":0,\"endRow\":0,\"totalRows\":0"));
		assertTrue(response.contains("The action you requested cannot be performed because: "));
		assertTrue(response.contains("Cannot approve this record"));
		assertFalse(response.contains(WebErrorUtil.GENERIC_ERROR_MESSAGE));
	}

	@Test
	@SuppressWarnings("static-method")
	void appendErrorTextWritesSynopsisAndBulletList() {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.appendErrorText("Problem:", List.of(new Message("First"), new Message("Second")), printWriter);
		}

		String text = writer.toString();
		assertTrue(text.contains("Problem:"));
		assertTrue(text.contains("<ul>"));
		assertTrue(text.contains("<li>First</li>"));
		assertTrue(text.contains("<li>Second</li>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void pumpOutValidationErrorsSanitisesBindingsAndDetectsPresence() throws Exception {
		StringBuilder builder = new StringBuilder();
		Method method = SmartClientEditServlet.class.getDeclaredMethod("pumpOutValidationErrors", List.class, StringBuilder.class);
		method.setAccessible(true);

		boolean hasBindings = ((Boolean) method.invoke(null,
				List.of(new Message(new String[] {"customer.name", "items[0].value"}, "Field is invalid")),
				builder)).booleanValue();

		assertTrue(hasBindings);
		assertEquals("\"customer_name\":\"Field is invalid\",\"items_0__value\":\"Field is invalid\",", builder.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void pumpOutValidationErrorsReturnsFalseWhenNoBindingsPresent() throws Exception {
		StringBuilder builder = new StringBuilder();
		Method method = SmartClientEditServlet.class.getDeclaredMethod("pumpOutValidationErrors", List.class, StringBuilder.class);
		method.setAccessible(true);

		boolean hasBindings = ((Boolean) method.invoke(null, List.of(new Message("No binding")), builder)).booleanValue();

		assertFalse(hasBindings);
		assertEquals("", builder.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void boundMessageExceptionWithMultipleBindingsPreservesAllFieldErrors() {
		String response = produce(new TestMessageException(
				new Message(new String[] {"customer.name", "customer.address.line1"}, "Missing value"),
				new Message(new String[] {"customer.email"}, "Invalid format")),
				Operation.update,
				true,
				REFERENCE);

		assertTrue(response.contains("\"customer_name\":\"Missing value\""));
		assertTrue(response.contains("\"customer_address_line1\":\"Missing value\""));
		assertTrue(response.contains("\"customer_email\":\"Invalid format\""));
		assertTrue(response.contains("\"status\":-4"));
		assertFalse(response.contains(WebErrorUtil.GENERIC_ERROR_MESSAGE));
	}

	private static String produce(Throwable t, Operation operation, boolean includeBindings, String reference) {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.produceErrorResponse(t, operation, includeBindings, printWriter, reference);
		}
		return writer.toString();
	}

	private static final class TestMessageException extends RuntimeException implements MessageException {
		private static final long serialVersionUID = 1L;

		private final List<Message> messages;

		private TestMessageException(Message... messages) {
			this.messages = List.of(messages);
		}

		@Override
		public List<Message> getMessages() {
			return messages;
		}
	}
}
