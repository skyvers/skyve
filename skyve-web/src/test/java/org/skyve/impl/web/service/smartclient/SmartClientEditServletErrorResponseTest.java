package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.SortedMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.metadata.view.View;
import org.skyve.util.OWASP;

import jakarta.servlet.http.HttpServletRequest;

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
	void unboundMessageExceptionForNonFetchOmitsPagingMetadata() {
		String response = produce(new TestMessageException(new Message("Cannot update this record")),
				Operation.update,
				true,
				REFERENCE);

		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains("Cannot update this record"));
		assertFalse(response.contains("startRow"));
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
	void appendErrorTextEscapesJavascriptSensitiveCharacters() {
		String raw = "\"quoted\" <script>";
		String expected = OWASP.escapeJsString(raw);

		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.appendErrorText("Problem:", List.of(new Message(raw)), printWriter);
		}

		String text = writer.toString();
		assertTrue(text.contains("<li>" + expected + "</li>"));
		assertFalse(text.contains(raw));
	}

	@Test
	@SuppressWarnings("static-method")
	void appendErrorTextHandlesEmptyMessageList() {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.appendErrorText("Problem:", List.of(), printWriter);
		}

		String text = writer.toString();
		assertTrue(text.contains("Problem:<br/><ul></ul>"));
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
	void pumpOutValidationErrorsEscapesMessageText() throws Exception {
		StringBuilder builder = new StringBuilder();
		Method method = SmartClientEditServlet.class.getDeclaredMethod("pumpOutValidationErrors", List.class, StringBuilder.class);
		method.setAccessible(true);
		String raw = "\"quoted\" <script>";
		String escaped = OWASP.escapeJsString(raw);

		boolean hasBindings = ((Boolean) method.invoke(null,
				List.of(new Message(new String[] {"customer.name"}, raw)),
				builder)).booleanValue();

		assertTrue(hasBindings);
		assertTrue(builder.toString().contains(escaped));
		assertTrue(builder.toString().contains("customer_name"));
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

	@Test
	@SuppressWarnings("static-method")
	void boundMessageExceptionWithoutBindingsFallsBackToGenericErrorStructure() {
		String response = produce(new TestMessageException(new Message("customer.name", "Name is required")),
				Operation.update,
				false,
				REFERENCE);

		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains("The action you requested cannot be performed because: "));
		assertTrue(response.contains("Name is required"));
		assertFalse(response.contains("\"status\":-4"));
	}

	@Test
	@SuppressWarnings("static-method")
	void technicalErrorOverloadIncludesGenericMessage() {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.produceErrorResponse(new IllegalStateException("sensitive detail"),
					Operation.update,
					true,
					printWriter);
		}

		String response = writer.toString();
		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains(WebErrorUtil.GENERIC_ERROR_MESSAGE));
		assertFalse(response.contains("sensitive detail"));
	}

	@Test
	@SuppressWarnings("static-method")
	void collectRequestParametersFiltersSystemAndNormalisesValues() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		List<String> names = List.of("isc_metaDataPrefix", "isc_dataFormat", "_operationType", "customer_name", "items_0__value", "emptyParam", "nullParam");
		Enumeration<String> enumeration = Collections.enumeration(names);

		when(request.getParameterNames()).thenReturn(enumeration);
		when(request.getParameter("customer_name")).thenReturn("Alice");
		when(request.getParameter("items_0__value")).thenReturn("42");
		when(request.getParameter("emptyParam")).thenReturn("");
		when(request.getParameter("nullParam")).thenReturn("null");
		when(request.getParameter("isc_metaDataPrefix")).thenReturn("meta");
		when(request.getParameter("isc_dataFormat")).thenReturn("json");
		when(request.getParameter("_operationType")).thenReturn("fetch");

		SortedMap<String, Object> result = SmartClientEditServlet.collectRequestParameters(request);

		assertEquals(4, result.size());
		assertEquals("Alice", result.get("customer.name"));
		assertEquals("42", result.get("items[0].value"));
		assertEquals(null, result.get("emptyParam"));
		assertEquals(null, result.get("nullParam"));
		assertFalse(result.containsKey("isc_metaDataPrefix"));
		assertFalse(result.containsKey("isc_dataFormat"));
		assertFalse(result.containsKey("_operationType"));
	}

	@Test
	@SuppressWarnings("static-method")
	void produceErrorResponseWithMixedBoundAndUnboundMessagesUsesBoundPathOnly() {
		String response = produce(new TestMessageException(
				new Message(new String[] {"customer.name"}, "Name is required"),
				new Message("A non-bound warning")),
				Operation.update,
				true,
				REFERENCE);

		assertTrue(response.contains("\"status\":-4"));
		assertTrue(response.contains("\"customer_name\":\"Name is required\""));
		assertFalse(response.contains("A non-bound warning"));
	}

	@Test
	@SuppressWarnings("static-method")
	void technicalErrorOverloadForFetchIncludesPagingMetadata() {
		StringWriter writer = new StringWriter();
		try (PrintWriter printWriter = new PrintWriter(writer)) {
			SmartClientEditServlet.produceErrorResponse(new IllegalStateException("sensitive detail"),
					Operation.fetch,
					true,
					printWriter);
		}

		String response = writer.toString();
		assertTrue(response.contains("\"status\":-1"));
		assertTrue(response.contains("\"startRow\":0,\"endRow\":0,\"totalRows\":0"));
		assertFalse(response.contains("sensitive detail"));
	}

	@Test
	@SuppressWarnings("static-method")
	void collectRequestParametersKeepsNonSystemUnderscoreNames() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Enumeration<String> enumeration = Collections.enumeration(List.of("name_with_underscore", "_csrf"));

		when(request.getParameterNames()).thenReturn(enumeration);
		when(request.getParameter("name_with_underscore")).thenReturn("value");
		when(request.getParameter("_csrf")).thenReturn("123");

		SortedMap<String, Object> result = SmartClientEditServlet.collectRequestParameters(request);

		assertEquals(1, result.size());
		assertEquals("value", result.get("name.with.underscore"));
		assertFalse(result.containsKey("_csrf"));
	}

	@Test
	@SuppressWarnings("static-method")
	void collectRequestParametersPreservesNullValueFromRequest() {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Enumeration<String> enumeration = Collections.enumeration(List.of("nullable_param"));

		when(request.getParameterNames()).thenReturn(enumeration);
		when(request.getParameter("nullable_param")).thenReturn(null);

		SortedMap<String, Object> result = SmartClientEditServlet.collectRequestParameters(request);

		assertEquals(1, result.size());
		assertEquals(null, result.get("nullable.param"));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void applyNewParametersUsesEditViewWhenBeanIsCreated() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(true).when(processBean).isCreated();
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(view.getParameters()).thenReturn(List.<View.ViewParameter>of());

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				new java.util.TreeMap<>(),
				"ux");

		verify(processDocument).getView("ux", customer, "edit");
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void applyNewParametersUsesCreateViewWhenBeanIsNotCreated() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(false).when(processBean).isCreated();
		when(processDocument.getView("ux", customer, "create")).thenReturn(view);
		when(view.getParameters()).thenReturn(List.<View.ViewParameter>of());

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				new java.util.TreeMap<>(),
				"ux");

		verify(processDocument).getView("ux", customer, "create");
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void applyNewParametersSkipsWhenIncomingParameterNotDeclaredInView() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(true).when(processBean).isCreated();
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("allowed.input");
		declared.setBoundTo("target.value");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("other.input", "abc");

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		verify(processDocument).getView("ux", customer, "edit");
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
