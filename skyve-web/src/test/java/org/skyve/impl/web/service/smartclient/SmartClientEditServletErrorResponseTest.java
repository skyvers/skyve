package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.SortedMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.Bean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.view.View;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.user.User;
import org.skyve.util.OWASP;

import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings({"java:S5778", "java:S1130"})
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
		String escaped = OWASP.escapeJsString(OWASP.escapeHtml(raw));

		boolean hasBindings = ((Boolean) method.invoke(null,
				List.of(new Message(new String[] {"customer.name"}, raw)),
				builder)).booleanValue();

		assertTrue(hasBindings);
		assertTrue(builder.toString().contains(escaped));
		assertTrue(builder.toString().contains("customer_name"));
		assertFalse(builder.toString().contains(raw));
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
	@SuppressWarnings("static-method")
	void applyNewParametersUsesEditViewWhenBeanIsCreated() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(Boolean.TRUE).when(processBean).isCreated();
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
	@SuppressWarnings("static-method")
	void applyNewParametersUsesCreateViewWhenBeanIsNotCreated() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(Boolean.FALSE).when(processBean).isCreated();
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
	@SuppressWarnings("static-method")
	void applyNewParametersSkipsWhenIncomingParameterNotDeclaredInView() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.domain.Bean processBean = mock(org.skyve.domain.Bean.class);
		View view = mock(View.class);

		org.mockito.Mockito.doReturn(Boolean.TRUE).when(processBean).isCreated();
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

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersClearsBoundValueWhenDeclaredIncomingParameterIsNull() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("allowed.input");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("allowed.input", null);

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		assertEquals(null, processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersCopiesImplicitBindingValueWithoutMetadataConversion() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("bizId")).thenReturn(null);
		when(processDocument.getExtends()).thenReturn(null);

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("bizId");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("bizId", "abc123");

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		assertEquals("abc123", processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersConvertsExplicitTextFieldValue() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);
		Text text = new Text();
		text.setName("name");

		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("name")).thenReturn(text);
		when(processDocument.getExtends()).thenReturn(null);

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("name");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("name", "Alice");

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		assertEquals("Alice", processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersSetsNullWhenAssociationLookupMisses() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.metadata.model.document.Document relatedDocument = mock(org.skyve.metadata.model.document.Document.class);
		Association association = mock(Association.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(module.getName()).thenReturn("admin");
		when(processDocument.getName()).thenReturn("Contact");
		when(processDocument.getOwningModuleName()).thenReturn("admin");
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("manager")).thenReturn(association);
		when(processDocument.getExtends()).thenReturn(null);
		when(association.getName()).thenReturn("manager");
		when(association.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		doReturn(String.class).when(association).getImplementingType();
		when(processDocument.getRelatedDocument(customer, "manager")).thenReturn(relatedDocument);
		when(persistence.retrieve(relatedDocument, "ref-1")).thenReturn(null);

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("manager");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("manager", "ref-1");

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		assertEquals(null, processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersThrowsWhenAssociationBeanIsNotReadable() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.metadata.model.document.Document relatedDocument = mock(org.skyve.metadata.model.document.Document.class);
		Association association = mock(Association.class);
		Bean relatedBean = mock(Bean.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(module.getName()).thenReturn("admin");
		when(processDocument.getName()).thenReturn("Contact");
		when(processDocument.getOwningModuleName()).thenReturn("admin");
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("manager")).thenReturn(association);
		when(processDocument.getExtends()).thenReturn(null);
		when(association.getName()).thenReturn("manager");
		when(association.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		doReturn(String.class).when(association).getImplementingType();
		when(processDocument.getRelatedDocument(customer, "manager")).thenReturn(relatedDocument);
		when(persistence.retrieve(relatedDocument, "ref-2")).thenReturn(relatedBean);
		when(relatedBean.getBizId()).thenReturn("rb1");
		when(relatedBean.getBizModule()).thenReturn("admin");
		when(relatedBean.getBizDocument()).thenReturn("Contact");
		when(relatedBean.getBizCustomer()).thenReturn("cust");
		when(relatedBean.getBizDataGroupId()).thenReturn("dg");
		when(relatedBean.getBizUserId()).thenReturn("user1");
		doReturn(Boolean.FALSE).when(user).canReadBean("rb1", "admin", "Contact", "cust", "dg", "user1");

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("manager");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("manager", "ref-2");

		assertThrows(RuntimeException.class,
				() -> SmartClientEditServlet.applyNewParameters(customer,
						user,
						persistence,
						module,
						processDocument,
						processBean,
						parameters,
						"ux"));

		verify(user).canReadBean("rb1", "admin", "Contact", "cust", "dg", "user1");
		assertEquals("before", processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersAppliesAssociationBeanWhenReadable() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.metadata.model.document.Document relatedDocument = mock(org.skyve.metadata.model.document.Document.class);
		Association association = mock(Association.class);
		Bean relatedBean = mock(Bean.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(module.getName()).thenReturn("admin");
		when(processDocument.getName()).thenReturn("Contact");
		when(processDocument.getOwningModuleName()).thenReturn("admin");
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("manager")).thenReturn(association);
		when(processDocument.getExtends()).thenReturn(null);
		when(association.getName()).thenReturn("manager");
		when(association.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		doReturn(String.class).when(association).getImplementingType();
		when(processDocument.getRelatedDocument(customer, "manager")).thenReturn(relatedDocument);
		when(persistence.retrieve(relatedDocument, "ref-3")).thenReturn(relatedBean);
		when(relatedBean.getBizId()).thenReturn("rb2");
		when(relatedBean.getBizModule()).thenReturn("admin");
		when(relatedBean.getBizDocument()).thenReturn("Contact");
		when(relatedBean.getBizCustomer()).thenReturn("cust");
		when(relatedBean.getBizDataGroupId()).thenReturn("dg");
		when(relatedBean.getBizUserId()).thenReturn("user2");
		doReturn(Boolean.TRUE).when(user).canReadBean("rb2", "admin", "Contact", "cust", "dg", "user2");

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("manager");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("manager", "ref-3");

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		verify(user).canReadBean("rb2", "admin", "Contact", "cust", "dg", "user2");
		assertEquals(relatedBean, processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void applyNewParametersUsesToStringPathForAssociationWhenIncomingValueIsNotString() throws Exception {
		org.skyve.metadata.customer.Customer customer = mock(org.skyve.metadata.customer.Customer.class);
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		org.skyve.metadata.module.Module module = mock(org.skyve.metadata.module.Module.class);
		org.skyve.metadata.model.document.Document processDocument = mock(org.skyve.metadata.model.document.Document.class);
		Association association = mock(Association.class);
		View view = mock(View.class);
		java.util.HashMap<String, Object> values = new java.util.HashMap<>();
		values.put("targetValue", "before");
		DynamicBean processBean = new DynamicBean("admin", "Contact", values);

		when(module.getName()).thenReturn("admin");
		when(processDocument.getName()).thenReturn("Contact");
		when(processDocument.getOwningModuleName()).thenReturn("admin");
		when(processDocument.getView("ux", customer, "edit")).thenReturn(view);
		when(processDocument.getAttribute("manager")).thenReturn(association);
		when(processDocument.getExtends()).thenReturn(null);
		when(association.getAttributeType()).thenReturn(org.skyve.metadata.model.Attribute.AttributeType.text);
		doReturn(String.class).when(association).getImplementingType();

		View.ViewParameter declared = new View.ViewParameter();
		declared.setFromBinding("manager");
		declared.setBoundTo("targetValue");
		when(view.getParameters()).thenReturn(List.of(declared));

		Object nonStringValue = new Object() {
			@Override
			public String toString() {
				return "converted-value";
			}
		};

		java.util.SortedMap<String, Object> parameters = new java.util.TreeMap<>();
		parameters.put("manager", nonStringValue);

		SmartClientEditServlet.applyNewParameters(customer,
				user,
				persistence,
				module,
				processDocument,
				processBean,
				parameters,
				"ux");

		assertEquals("converted-value", processBean.get("targetValue"));
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateThrowsIllegalArgumentWhenSecurityExceptionLoggingNeedsPersistence() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		doReturn(Boolean.FALSE).when(user).canDeleteDocument(processDocument);

		assertThrows(IllegalArgumentException.class,
				() -> invokeRemovePrivate(webContext,
						user,
						customer,
						processDocument,
						beanToDelete,
						null,
						persistence,
						new PrintWriter(new StringWriter())));
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateThrowsValidationWhenBeanAlreadyDeleted() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("gone-1");
		when(persistence.retrieve(processDocument, "gone-1")).thenReturn(null);

		assertThrows(ValidationException.class,
				() -> invokeRemovePrivate(webContext,
						user,
						customer,
						processDocument,
						beanToDelete,
						null,
						persistence,
						new PrintWriter(new StringWriter())));
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateThrowsIllegalArgumentWhenReadSecurityLoggingNeedsPersistence() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b1");
		when(persistence.retrieve(processDocument, "b1")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b1");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		doReturn(Boolean.FALSE).when(user).canReadBean("b1", "admin", "Contact", "cust", "dg", "user1");

		assertThrows(IllegalArgumentException.class,
				() -> invokeRemovePrivate(webContext,
						user,
						customer,
						processDocument,
						beanToDelete,
						null,
						persistence,
						new PrintWriter(new StringWriter())));
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateThrowsOptimisticLockWhenLockMismatch() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b2");
		when(beanToDelete.getBizLock()).thenReturn(new OptimisticLock("u1", new java.util.Date(10L)));
		when(persistence.retrieve(processDocument, "b2")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b2");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		doReturn(Boolean.TRUE).when(user).canReadBean("b2", "admin", "Contact", "cust", "dg", "user1");
		when(retrieved.getBizLock()).thenReturn(new OptimisticLock("u2", new java.util.Date(11L)));

		assertThrows(OptimisticLockException.class,
				() -> invokeRemovePrivate(webContext,
						user,
						customer,
						processDocument,
						beanToDelete,
						null,
						persistence,
						new PrintWriter(new StringWriter())));
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateDeletesAndWritesSuccessPayloadOnHappyPath() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		StringWriter sink = new StringWriter();
		PrintWriter pw = new PrintWriter(sink);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b3");
		OptimisticLock lock = new OptimisticLock("u3", new java.util.Date(12L));
		when(beanToDelete.getBizLock()).thenReturn(lock);
		when(persistence.retrieve(processDocument, "b3")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b3");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		when(retrieved.getBizLock()).thenReturn(lock);
		doReturn(Boolean.TRUE).when(user).canReadBean("b3", "admin", "Contact", "cust", "dg", "user1");
		doReturn(Boolean.TRUE).when(customer).interceptBeforePreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		doReturn(Boolean.TRUE).when(customer).interceptBeforePostRender(retrieved, webContext);

		assertThrows(NullPointerException.class,
				() -> invokeRemovePrivate(webContext, user, customer, processDocument, beanToDelete, null, persistence, pw));
		pw.flush();

		verify(persistence).delete(processDocument, retrieved);
		assertEquals("{\"response\":{\"status\":0}}", sink.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateInvokesBizletPreExecuteWhenNotVetoed() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		Bizlet<PersistentBean> bizlet = mock(Bizlet.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		StringWriter sink = new StringWriter();
		PrintWriter pw = new PrintWriter(sink);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b4");
		OptimisticLock lock = new OptimisticLock("u4", new java.util.Date(13L));
		when(beanToDelete.getBizLock()).thenReturn(lock);
		when(persistence.retrieve(processDocument, "b4")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b4");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		when(retrieved.getBizLock()).thenReturn(lock);
		doReturn(Boolean.TRUE).when(user).canReadBean("b4", "admin", "Contact", "cust", "dg", "user1");
		doReturn(Boolean.FALSE).when(customer).interceptBeforePreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		when(bizlet.preExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext)).thenReturn(retrieved);

		assertThrows(NullPointerException.class,
				() -> invokeRemovePrivate(webContext, user, customer, processDocument, beanToDelete, bizlet, persistence, pw));
		pw.flush();

		verify(bizlet).preExecute(org.skyve.metadata.controller.ImplicitActionName.Delete, retrieved, null, webContext);
		verify(customer).interceptAfterPreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		verify(persistence).delete(processDocument, retrieved);
		assertEquals("{\"response\":{\"status\":0}}", sink.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateInvokesAfterPreExecuteWhenNotVetoedAndBizletIsNull() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		StringWriter sink = new StringWriter();
		PrintWriter pw = new PrintWriter(sink);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b4n");
		OptimisticLock lock = new OptimisticLock("u4n", new java.util.Date(15L));
		when(beanToDelete.getBizLock()).thenReturn(lock);
		when(persistence.retrieve(processDocument, "b4n")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b4n");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		when(retrieved.getBizLock()).thenReturn(lock);
		doReturn(Boolean.TRUE).when(user).canReadBean("b4n", "admin", "Contact", "cust", "dg", "user1");
		doReturn(Boolean.FALSE).when(customer).interceptBeforePreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		doReturn(Boolean.TRUE).when(customer).interceptBeforePostRender(retrieved, webContext);

		assertThrows(NullPointerException.class,
				() -> invokeRemovePrivate(webContext, user, customer, processDocument, beanToDelete, null, persistence, pw));
		pw.flush();

		verify(customer).interceptAfterPreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		verify(persistence).delete(processDocument, retrieved);
		assertEquals("{\"response\":{\"status\":0}}", sink.toString());
	}

	@Test
	@SuppressWarnings("static-method")
	void removePrivateSkipsBizletPreExecuteWhenVetoed() throws Exception {
		AbstractWebContext webContext = mock(AbstractWebContext.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Document processDocument = mock(Document.class);
		PersistentBean beanToDelete = mock(PersistentBean.class);
		PersistentBean retrieved = mock(PersistentBean.class);
		Bizlet<PersistentBean> bizlet = mock(Bizlet.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);

		doReturn(Boolean.TRUE).when(user).canDeleteDocument(processDocument);
		when(beanToDelete.getBizId()).thenReturn("b5");
		OptimisticLock lock = new OptimisticLock("u5", new java.util.Date(14L));
		when(beanToDelete.getBizLock()).thenReturn(lock);
		when(persistence.retrieve(processDocument, "b5")).thenReturn(retrieved);
		when(retrieved.getBizId()).thenReturn("b5");
		when(retrieved.getBizModule()).thenReturn("admin");
		when(retrieved.getBizDocument()).thenReturn("Contact");
		when(retrieved.getBizCustomer()).thenReturn("cust");
		when(retrieved.getBizDataGroupId()).thenReturn("dg");
		when(retrieved.getBizUserId()).thenReturn("user1");
		when(retrieved.getBizLock()).thenReturn(lock);
		doReturn(Boolean.TRUE).when(user).canReadBean("b5", "admin", "Contact", "cust", "dg", "user1");
		doReturn(Boolean.TRUE).when(customer).interceptBeforePreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);

		assertThrows(NullPointerException.class,
				() -> invokeRemovePrivate(webContext,
						user,
						customer,
						processDocument,
						beanToDelete,
						bizlet,
						persistence,
						new PrintWriter(new StringWriter())));

		verify(bizlet, never()).preExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		verify(customer, never()).interceptAfterPreExecute(org.skyve.metadata.controller.ImplicitActionName.Delete,
				retrieved,
				null,
				webContext);
		verify(persistence).delete(processDocument, retrieved);
	}

	@Test
	@SuppressWarnings("static-method")
	void postRenderPrivateSkipsBizletAndAfterWhenVetoed() throws Exception {
		CustomerImpl customer = mock(CustomerImpl.class);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Bean bean = mock(Bean.class);
		AbstractWebContext webContext = mock(AbstractWebContext.class);

		doReturn(Boolean.TRUE).when(customer).interceptBeforePostRender(bean, webContext);

		invokePostRenderPrivate(customer, bizlet, bean, webContext);

		verify(bizlet, never()).postRender(bean, webContext);
		verify(customer, never()).interceptAfterPostRender(bean, webContext);
	}

	@Test
	@SuppressWarnings("static-method")
	void postRenderPrivateRunsBizletAndAfterWhenNotVetoed() throws Exception {
		CustomerImpl customer = mock(CustomerImpl.class);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Bean bean = mock(Bean.class);
		AbstractWebContext webContext = mock(AbstractWebContext.class);

		doReturn(Boolean.FALSE).when(customer).interceptBeforePostRender(bean, webContext);

		invokePostRenderPrivate(customer, bizlet, bean, webContext);

		verify(bizlet).postRender(bean, webContext);
		verify(customer).interceptAfterPostRender(bean, webContext);
	}

	@Test
	@SuppressWarnings("static-method")
	void postRenderPrivateRunsAfterWhenNotVetoedAndBizletIsNull() throws Exception {
		CustomerImpl customer = mock(CustomerImpl.class);
		Bean bean = mock(Bean.class);
		AbstractWebContext webContext = mock(AbstractWebContext.class);

		doReturn(Boolean.FALSE).when(customer).interceptBeforePostRender(bean, webContext);

		invokePostRenderPrivate(customer, null, bean, webContext);

		verify(customer).interceptAfterPostRender(bean, webContext);
	}

	private static void invokeRemovePrivate(AbstractWebContext webContext,
										User user,
										CustomerImpl customer,
										Document processDocument,
										PersistentBean beanToDelete,
										Bizlet<PersistentBean> bizlet,
										AbstractPersistence persistence,
										PrintWriter pw) throws Exception {
		Method method = SmartClientEditServlet.class.getDeclaredMethod("remove",
				AbstractWebContext.class,
				User.class,
				org.skyve.metadata.customer.Customer.class,
				Document.class,
				PersistentBean.class,
				Bizlet.class,
				AbstractPersistence.class,
				PrintWriter.class);
		method.setAccessible(true);
		try {
			method.invoke(null, webContext, user, customer, processDocument, beanToDelete, bizlet, persistence, pw);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void invokePostRenderPrivate(CustomerImpl customer,
										Bizlet<Bean> bizlet,
										Bean bean,
										AbstractWebContext webContext) throws Exception {
		Method method = SmartClientEditServlet.class.getDeclaredMethod("postRender",
				CustomerImpl.class,
				Bizlet.class,
				Bean.class,
				AbstractWebContext.class);
		method.setAccessible(true);
		try {
			method.invoke(null, customer, bizlet, bean, webContext);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
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
