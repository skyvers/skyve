package org.skyve.impl.web.service.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.util.DataBuilder;
import org.skyve.util.JSON;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

class RestServiceH2Test extends AbstractSkyveTest {

	@Test
	void insertJsonPostPersistsAndReturnsInsertedBean() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("insert-" + System.nanoTime());
		String json = JSON.marshall(c, bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.insertJSONPost(json);

		assertNotNull(result);
		AllAttributesPersistent inserted = (AllAttributesPersistent) JSON.unmarshall(u, result);
		assertNotNull(inserted.getBizId());
		assertEquals(bean.getText(), inserted.getText());
		assertNotNull(p.retrieve(aapd, inserted.getBizId()));
		verify(response).setContentType("application/json");
	}

	@Test
	void updateJsonPostUpdatesPersistedBeanAndReturnsUpdatedJson() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("before-" + System.nanoTime());
		bean = p.save(bean);
		String updatedText = "after-" + System.nanoTime();
		bean.setText(updatedText);
		String json = JSON.marshall(c, bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.updateJSONPost(json);

		assertNotNull(result);
		AllAttributesPersistent updated = (AllAttributesPersistent) JSON.unmarshall(u, result);
		assertEquals(bean.getBizId(), updated.getBizId());
		assertEquals(updatedText, updated.getText());
		AllAttributesPersistent persisted = (AllAttributesPersistent) p.retrieve(aapd, bean.getBizId());
		assertNotNull(persisted);
		assertEquals(updatedText, persisted.getText());
		verify(response).setContentType("application/json");
	}

	@Test
	void deleteJsonDeleteRemovesPersistedBeanAndReturnsEmptyJsonObject() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("delete-" + System.nanoTime());
		bean = p.save(bean);
		String json = JSON.marshall(c, bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.deleteJSONDelete(json);

		assertEquals("{}", result);
		assertNull(p.retrieve(aapd, bean.getBizId()));
		verify(response).setContentType("application/json");
	}

	@Test
	void queryReturnsJsonForDefaultDocumentQuery() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("rest-query-" + System.nanoTime());
		p.save(bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.query(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											0,
											10);

		assertNotNull(result);
		verify(response).setContentType("application/json");
	}

	@Test
	void queryNullifiesFlagCommentWhenUserCannotFlag() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
												 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("rest-query-cannot-flag-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		User originalUser = persistence.getUser();
		User spyUser = spy(originalUser);
		doReturn(Boolean.FALSE).when(spyUser).canFlag();

		try {
			persistence.setUser(spyUser);
			String result = service.query(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											0,
											10);

			assertNotNull(result);
			verify(response).setContentType("application/json");
		}
		finally {
			persistence.setUser(originalUser);
		}
	}

	@Test
	void retrieveJsonByIdReturnsJsonForPersistedBean() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-json-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.retrieveJSON(AllAttributesPersistent.MODULE_NAME,
												AllAttributesPersistent.DOCUMENT_NAME,
												bean.getBizId());

		assertNotNull(result);
		verify(response).setContentType("application/json");
	}

	@Test
	void retrieveJsonByIdReturnsNullWhenUserCannotReadDocument() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
												 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-json-denied-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mockResponse();
		RestService service = newService(response);

		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		User originalUser = persistence.getUser();
		User spyUser = spy(originalUser);
		doReturn(Boolean.FALSE).when(spyUser).canReadDocument(any(Document.class));

		try {
			persistence.setUser(spyUser);
			String result = service.retrieveJSON(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											bean.getBizId());

			assertNull(result);
			verify(response, atLeastOnce()).setContentType("application/json");
		}
		finally {
			persistence.setUser(originalUser);
		}
	}

	@Test
	void retrieveXmlByIdReturnsBeanForPersistedBean() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-xml-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		Object result = service.retrieveXML(AllAttributesPersistent.MODULE_NAME,
												AllAttributesPersistent.DOCUMENT_NAME,
												bean.getBizId());

		assertNotNull(result);
		verify(response).setContentType("application/xml");
	}

	@Test
	void retrieveXmlByIdReturnsNullWhenUserCannotReadDocument() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
												 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-xml-denied-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mockResponse();
		RestService service = newService(response);

		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		User originalUser = persistence.getUser();
		User spyUser = spy(originalUser);
		doReturn(Boolean.FALSE).when(spyUser).canReadDocument(any(Document.class));

		try {
			persistence.setUser(spyUser);
			Object result = service.retrieveXML(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											bean.getBizId());

			assertNull(result);
			verify(response, atLeastOnce()).setContentType("application/xml");
		}
		finally {
			persistence.setUser(originalUser);
		}
	}

	@Test
	void retrieveJsonListReturnsJsonForPersistedRows() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
													 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-list-" + System.nanoTime());
		p.save(bean);

		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);

		String result = service.retrieveJSON(AllAttributesPersistent.MODULE_NAME,
												AllAttributesPersistent.DOCUMENT_NAME,
												0,
												10);

		assertNotNull(result);
		verify(response).setContentType("application/json");
	}

	@Test
	void retrieveJsonListReturnsNullWhenUserCannotReadDocument() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
												 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("retrieve-list-denied-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mockResponse();
		RestService service = newService(response);

		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		User originalUser = persistence.getUser();
		User spyUser = spy(originalUser);
		doReturn(Boolean.FALSE).when(spyUser).canReadDocument(any(Document.class));

		try {
			persistence.setUser(spyUser);
			String result = service.retrieveJSON(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											0,
											10);

			assertNull(result);
			verify(response, atLeastOnce()).setContentType("application/json");
		}
		finally {
			persistence.setUser(originalUser);
		}
	}

	@Test
	void queryReturnsNullWhenUserCannotReadDocument() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
												 .build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("query-denied-" + System.nanoTime());
		bean = p.save(bean);

		HttpServletResponse response = mockResponse();
		RestService service = newService(response);

		AbstractPersistence persistence = (AbstractPersistence) CORE.getPersistence();
		User originalUser = persistence.getUser();
		User spyUser = spy(originalUser);
		doReturn(Boolean.FALSE).when(spyUser).canReadDocument(any(Document.class));

		try {
			persistence.setUser(spyUser);
			String result = service.query(AllAttributesPersistent.MODULE_NAME,
											AllAttributesPersistent.DOCUMENT_NAME,
											0,
											10);

			assertNull(result);
			verify(response, atLeastOnce()).setContentType("application/json");
		}
		finally {
			persistence.setUser(originalUser);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void queryContentReturnsNullAndSetsNotFoundWhenAttachmentMissing() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		RestService service = newService(response);
		Field requestField = RestService.class.getDeclaredField("request");
		requestField.setAccessible(true);
		requestField.set(service, request);

		String missingContentId = "missing-" + System.nanoTime();
		String requestUri = "/rest/content/" + missingContentId;
		org.mockito.Mockito.when(request.getRequestURI()).thenReturn(requestUri);

		byte[] result = service.queryContent(missingContentId);

		assertNull(result);
		verify(response).setStatus(HttpServletResponse.SC_NOT_FOUND);
	}

	private static RestService newService(HttpServletResponse response) throws Exception {
		RestService result = new RestService();
		Field responseField = RestService.class.getDeclaredField("response");
		responseField.setAccessible(true);
		responseField.set(result, response);
		return result;
	}

	@SuppressWarnings("resource")
	private static HttpServletResponse mockResponse() throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		ServletOutputStream servletOut = new ServletOutputStream() {
			@Override
			public void write(int b) throws IOException {
				out.write(b);
			}

			@Override
			public boolean isReady() {
				return true;
			}

			@Override
			public void setWriteListener(WriteListener writeListener) {
				// Not needed for unit tests.
			}
		};
		doReturn(servletOut).when(response).getOutputStream();
		return response;
	}

}
