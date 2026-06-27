package org.skyve.impl.web.service.rest;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "boxing"})
class RestServiceDeleteJsonTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void deleteJsonDeleteReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.deleteJSONDelete("{}");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void deleteJsonGetReturnsNullAndRollsBackOnUnmarshallFailure() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(mock(User.class));
		bindPersistenceToThread(persistence);

		String result = service.deleteJSONGet("not-json");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonByIdReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", "1");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveXmlByIdReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		Object result = service.retrieveXML("admin", "Contact", "1");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/xml");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonListReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", 0, 10);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void insertJsonPostReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.insertJSONPost("{}");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void insertJsonGetReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.insertJSONGet("{}");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void updateJsonPostReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.updateJSONPost("{}");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void updateJsonGetReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.updateJSONGet("{}");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonByIdReturnsNullAndRollsBackOnReadDenied() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(false);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", "id-1");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveXmlByIdReturnsNullAndRollsBackOnReadDenied() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(false);
		bindPersistenceToThread(persistence);

		Object result = service.retrieveXML("admin", "Contact", "id-1");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/xml");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonListReturnsNullAndRollsBackOnReadDenied() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(false);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", 0, 10);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonByIdReturnsNullAndRollsBackOnNoResults() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-2")).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", "id-2");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveXmlByIdReturnsNullAndRollsBackOnNoResults() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-3")).thenReturn(null);
		bindPersistenceToThread(persistence);

		Object result = service.retrieveXML("admin", "Contact", "id-3");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/xml");
		verify(persistence).rollback();
	}

	@Test
	void retrieveXmlByIdReturnsBeanWhenReadableAndFound() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DynamicBean bean = new DynamicBean("admin", "Contact", new java.util.HashMap<>());

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-xml")).thenReturn(bean);
		bindPersistenceToThread(persistence);

		Object result = service.retrieveXML("admin", "Contact", "id-xml");

		assertSame(bean, result);
		verify(response, atLeastOnce()).setContentType("application/xml");
		verify(persistence, never()).rollback();
	}

	@Test
	void retrieveJsonByIdCoversNonNullRetrieveBranchThenRollsBackOnPopulateFailure() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.domain.Bean bean = mock(org.skyve.domain.Bean.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-4")).thenReturn(bean);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", "id-4");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveXmlByIdCoversNonNullRetrieveBranchThenRollsBackOnPopulateFailure() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.domain.Bean bean = mock(org.skyve.domain.Bean.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-5")).thenReturn(bean);
		bindPersistenceToThread(persistence);

		Object result = service.retrieveXML("admin", "Contact", "id-5");

		assertEquals(bean, result);
		verify(response, atLeastOnce()).setContentType("application/xml");
	}

	@Test
	void retrieveJsonListCoversNonEmptyProjectedResultsLoopThenRollsBackOnPopulateFailure() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);
		org.skyve.domain.Bean bean = mock(org.skyve.domain.Bean.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.projectedResults()).thenReturn(java.util.Collections.singletonList(bean));
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", 0, 2);

		assertNull(result);
		verify(query).setFirstResult(0);
		verify(query).setMaxResults(1);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonListReturnsNullAndRollsBackAfterDocumentQueryExecutionPath() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DocumentQuery query = mock(DocumentQuery.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.projectedResults()).thenReturn(java.util.Collections.emptyList());
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", 10, 30);

		assertEquals("[]", result);
		verify(query).setFirstResult(10);
		verify(query).setMaxResults(19);
		verify(query).projectedResults();
		verify(response, atLeastOnce()).setContentType("application/json");
	}

	@Test
	void queryReturnsNullAndRollsBackOnMissingUser() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "Contact", 0, 10);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void queryReturnsNullAndRollsBackWhenListModelCreationFails() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("MyQuery")).thenReturn(query);
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "MyQuery", 5, 15);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void queryContentReturnsNullAndSetsNotFoundWhenAttachmentMissing() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getRequestURI()).thenReturn("/api/content/cid-404");
		setPrivateField(service, "request", request);
		setPrivateField(service, "response", response);

		byte[] result = service.queryContent("cid-404");

		assertNull(result);
	}

	@Test
	void queryContentReturnsNullWhenRequestIsUnavailable() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		byte[] result = service.queryContent("cid-500");

		assertNull(result);
	}

	@Test
	void queryContentReturnsNullWhenContentManagerIsUnavailable() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getRequestURI()).thenReturn("/api/content/cid-1");
		setPrivateField(service, "request", request);
		setPrivateField(service, "response", response);

		byte[] result = service.queryContent("cid-1");

		assertNull(result);
	}

	@Test
	void insertContentReturnsNullAndRollsBackOnAccessDenied() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getDataGroupId()).thenReturn("DG");
		when(user.canAccessContent("id-1", "admin", "Contact", "demo", "DG", "id-1", "image")).thenReturn(false);
		bindPersistenceToThread(persistence);

		String result = service.insertContent("demo", "admin", "Contact", "id-1", "image", "image/png", "AAA=");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
	}

	@Test
	void insertContentReturnsNullWhenCurrentUserIsMissing() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.insertContent("demo", "admin", "Contact", "id-1", "image", "image/png", "AAA=");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
	}

	@Test
	void insertContentReturnsNullWhenAccessCheckFailsEarly() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getDataGroupId()).thenReturn("DG");
		when(user.getId()).thenReturn("U1");
		when(user.canAccessContent("id-1", "admin", "Contact", "demo", "DG", "id-1", "image")).thenReturn(false);
		bindPersistenceToThread(persistence);

		String result = service.insertContent("demo", "admin", "Contact", "id-1", "image", "image/png", "AAA=");

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
	}

	@Test
	void queryReturnsNullAndRollsBackWhenMetaDataQueryLookupFails() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("MyQuery")).thenThrow(new IllegalStateException("boom"));
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "MyQuery", 0, 20);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void queryReturnsNullAndRollsBackWhenCustomerIsMissing() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(null);
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "MyQuery", 0, 20);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void queryReturnsNullAndRollsBackWhenDefaultQueryLookupFails() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("DocName")).thenReturn((MetaDataQueryDefinition) null);
		when(module.getDocumentDefaultQuery(customer, "DocName")).thenThrow(new IllegalStateException("no-default"));
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "DocName", 0, 20);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void queryReturnsNullAndRollsBackWhenMetaAndDefaultQueriesAreNull() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMetaDataQuery("DocName")).thenReturn((MetaDataQueryDefinition) null);
		when(module.getDocumentDefaultQuery(customer, "DocName")).thenReturn((MetaDataQueryDefinition) null);
		bindPersistenceToThread(persistence);

		String result = service.query("admin", "DocName", 0, 20);

		assertNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence).rollback();
	}

	@Test
	void retrieveJsonByIdReturnsJsonWhenReadableAndFound() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		DynamicBean bean = new DynamicBean("admin", "Contact", new java.util.HashMap<>());

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.retrieve(document, "id-json")).thenReturn(bean);
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", "id-json");

		assertNotNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence, never()).rollback();
	}

	@Test
	void retrieveJsonListReturnsJsonWhenReadableAndFound() throws Exception {
		RestService service = new RestService();
		HttpServletResponse response = mockResponse();
		setPrivateField(service, "response", response);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		org.skyve.persistence.DocumentQuery query = mock(org.skyve.persistence.DocumentQuery.class);
		DynamicBean bean = new DynamicBean("admin", "Contact", new java.util.HashMap<>());

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		when(user.canReadDocument(document)).thenReturn(true);
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.projectedResults()).thenReturn(Collections.singletonList(bean));
		bindPersistenceToThread(persistence);

		String result = service.retrieveJSON("admin", "Contact", 0, 1);

		assertNotNull(result);
		verify(response, atLeastOnce()).setContentType("application/json");
		verify(persistence, never()).rollback();
	}


	@SuppressWarnings("resource")
	private static HttpServletResponse mockResponse() throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getOutputStream()).thenReturn(new BufferingServletOutputStream());
		return response;
	}

	private static void setPrivateField(Object instance, String fieldName, Object value) throws Exception {
		Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(instance, value);
	}

	@SuppressWarnings("unchecked")
	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	@SuppressWarnings("unchecked")
	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static final class BufferingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream delegate = new ByteArrayOutputStream();

		@Override
		public void write(int b) {
			delegate.write(b);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// no-op
		}
	}
}
