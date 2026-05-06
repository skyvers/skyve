package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.web.WebErrorUtil;

class SmartClientEditServletErrorResponseTest {
	private static final String REFERENCE = "2f8f0e2c-3b93-4cc2-9d9d-5f24ec777a3d";

	@Test
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

		private TestMessageException(Message message) {
			messages = List.of(message);
		}

		@Override
		public List<Message> getMessages() {
			return messages;
		}
	}
}
