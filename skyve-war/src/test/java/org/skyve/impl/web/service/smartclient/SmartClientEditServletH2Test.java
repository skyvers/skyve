package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.ServletConstants;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

// Reflection and servlet exception propagation are required to exercise private servlet branches.
@SuppressWarnings({ "java:S1192", "java:S1989", "java:S3011", "java:S5960" })
class SmartClientEditServletH2Test extends AbstractSkyveTest {
	private static final String UXUI = "external";

	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void fetchCreatesTopLevelBeanAndRendersViewJson() throws Exception {
		String body = invokeFetch(null, null, null, new TreeMap<>());

		assertSuccessfulEditFetch(body);
	}

	@Test
	void fetchRetrievesPersistedTopLevelBeanAndRendersViewJson() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-fetch-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeFetch(bean.getBizId(), null, null, new TreeMap<>());

		assertSuccessfulEditFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void fetchCreatesTopLevelBeanForRerenderSource() throws Exception {
		String body = invokeFetch(null, "text", ImplicitActionName.OK, new TreeMap<>());

		assertSuccessfulEditFetch(body);
	}

	@Test
	void fetchRetrievesPersistedTopLevelBeanForRerenderSource() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-rerender-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeFetch(bean.getBizId(), "text", ImplicitActionName.OK, new TreeMap<>());

		assertSuccessfulEditFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void doGetReturnsEmptyResponseWhenOperationTypeMissing() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param(AbstractWebContext.MODULE_NAME, AllAttributesPersistent.MODULE_NAME)
				.param(AbstractWebContext.DOCUMENT_NAME, AllAttributesPersistent.DOCUMENT_NAME)).doGet();

		assertTrue(response.body().contains("{}"), response.body());
	}

	@Test
	void doPostDelegatesToEmptyResponseWhenOperationTypeMissing() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param(AbstractWebContext.MODULE_NAME, AllAttributesPersistent.MODULE_NAME)
				.param(AbstractWebContext.DOCUMENT_NAME, AllAttributesPersistent.DOCUMENT_NAME)).doPost();

		assertTrue(response.body().contains("{}"), response.body());
	}

	@Test
	void doGetReturnsSmartClientErrorWhenFetchCountersAreMissing() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("_operationType", Operation.fetch.toString())
				.param(AbstractWebContext.MODULE_NAME, AllAttributesPersistent.MODULE_NAME)
				.param(AbstractWebContext.DOCUMENT_NAME, AllAttributesPersistent.DOCUMENT_NAME)).doGet();

		assertTrue(response.body().contains("\"status\":-1"), response.body());
		assertTrue(response.body().contains("\"totalRows\":0"), response.body());
	}

	@Test
	void removeDeletesPersistedTopLevelBeanAndWritesSuccessPayload() throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText("edit-delete-" + System.nanoTime());
		bean = p.save(bean);

		String body = invokeRemove(bean);

		assertTrue(body.contains("\"status\":0"), body);
		assertNull(p.retrieve(aapd, bean.getBizId()));
	}

	@Test
	void pumpOutResponseRendersCurrentBeanWithRedirectUrl() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-response-" + System.nanoTime());
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		webContext.setCurrentBean(bean);
		View view = aapd.getView(UXUI, c, ViewType.create.toString());
		StringWriter sink = new StringWriter();

		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method pumpOutResponse = SmartClientEditServlet.class.getDeclaredMethod("pumpOutResponse",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					CustomerImpl.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					View.class,
					String.class,
					Bean.class,
					Bizlet.class,
					int.class,
					int.class,
					String.class,
					PrintWriter.class);
			pumpOutResponse.setAccessible(true);
			try {
				pumpOutResponse.invoke(null,
						webContext,
						u,
						c,
						m,
						aapd,
						view,
						UXUI,
						bean,
						null,
						Integer.valueOf(0),
						Integer.valueOf(0),
						"/download/test",
						writer);
			} catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}

		String body = sink.toString();
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
	}

	@Test
	void applyPushActionSkipsJsonApplyAndRendersResponse() throws Exception {
		AllAttributesPersistent bean = AllAttributesPersistent.newInstance();
		bean.setText("edit-push-" + System.nanoTime());
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		webContext.setCurrentBean(bean);
		StringWriter sink = new StringWriter();

		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method apply = SmartClientEditServlet.class.getDeclaredMethod("apply",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					Bean.class,
					org.skyve.metadata.model.document.Document.class,
					Bean.class,
					String.class,
					String.class,
					String.class,
					ImplicitActionName.class,
					String.class,
					int.class,
					int.class,
					SortedMap.class,
					AbstractPersistence.class,
					String.class,
					PrintWriter.class);
			apply.setAccessible(true);
			try {
				apply.invoke(null,
						webContext,
						u,
						c,
						m,
						aapd,
						bean,
						aapd,
						bean,
						null,
						null,
						null,
						null,
						ServletConstants.PUSH_ACTION_NAME,
						Integer.valueOf(0),
						Integer.valueOf(0),
						new TreeMap<>(),
						p,
						UXUI,
						writer);
			} catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}

		String body = sink.toString();
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
	}

	private String invokeFetch(String bizId,
			String source,
			ImplicitActionName action,
			SortedMap<String, Object> parameters)
			throws Exception {
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		StringWriter sink = new StringWriter();
		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method fetch = SmartClientEditServlet.class.getDeclaredMethod("fetch",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					Bean.class,
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					String.class,
					String.class,
					String.class,
					int.class,
					int.class,
					ImplicitActionName.class,
					SortedMap.class,
					AbstractPersistence.class,
					String.class,
					PrintWriter.class);
			fetch.setAccessible(true);
			try {
				fetch.invoke(null,
						webContext,
						u,
						c,
						null,
						m,
						aapd,
						null,
						source,
						bizId,
						Integer.valueOf(0),
						Integer.valueOf(0),
						action,
						parameters,
						p,
						UXUI,
						writer);
			} catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}
		return sink.toString();
	}

	private String invokeRemove(PersistentBean beanToDelete) throws Exception {
		AbstractWebContext webContext = mockWebContext();
		webContext.setConversation((AbstractPersistence) p);
		Bizlet<?> bizlet = ((DocumentImpl) aapd).getBizlet(c);
		StringWriter sink = new StringWriter();
		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method remove = SmartClientEditServlet.class.getDeclaredMethod("remove",
					AbstractWebContext.class,
					org.skyve.metadata.user.User.class,
					org.skyve.metadata.customer.Customer.class,
					org.skyve.metadata.model.document.Document.class,
					PersistentBean.class,
					Bizlet.class,
					AbstractPersistence.class,
					PrintWriter.class);
			remove.setAccessible(true);
			try {
				remove.invoke(null,
						webContext,
						u,
						c,
						aapd,
						beanToDelete,
						bizlet,
						p,
						writer);
			} catch (InvocationTargetException e) {
				Throwable cause = e.getCause();
				if (cause instanceof Exception exception) {
					throw exception;
				}
				throw e;
			}
		}
		return sink.toString();
	}

	private static void assertSuccessfulEditFetch(String body) {
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"totalRows\":1"), body);
		assertTrue(body.contains("\"data\":["), body);
	}

	private static ServletHarness service(RequestBuilder request) throws IOException {
		CapturedResponse response = new CapturedResponse();
		return new ServletHarness(request.build(), response);
	}

	private static RequestBuilder newRequest(User user) {
		return new RequestBuilder(user);
	}

	private static final class ServletHarness {
		private final TestableSmartClientEditServlet servlet = new TestableSmartClientEditServlet();
		private final HttpServletRequest request;
		private final CapturedResponse response;

		private ServletHarness(HttpServletRequest request, CapturedResponse response) {
			this.request = request;
			this.response = response;
		}

		private CapturedResponse doGet() throws ServletException, IOException {
			servlet.doGet(request, response.response);
			return response;
		}

		private CapturedResponse doPost() throws ServletException, IOException {
			servlet.doPost(request, response.response);
			return response;
		}
	}

	private static final class TestableSmartClientEditServlet extends SmartClientEditServlet {
		private static final long serialVersionUID = 1L;

		@Override
		public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			super.doGet(request, response);
		}

		@Override
		public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			super.doPost(request, response);
		}
	}

	private static final class RequestBuilder {
		private final Map<String, String[]> parameters = new LinkedHashMap<>();
		private final HttpSession session = mock(HttpSession.class);

		private RequestBuilder(User user) {
			when(session.getId()).thenReturn("smart-client-edit-session-" + System.nanoTime());
			when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
			StateUtil.addSession(user.getId(), session);
		}

		private RequestBuilder param(String name, String value) {
			parameters.put(name, new String[] { value });
			return this;
		}

		private HttpServletRequest build() {
			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getParameterNames()).thenReturn(parameterNames());
			when(request.getParameterMap()).thenReturn(Collections.unmodifiableMap(parameters));
			when(request.getSession()).thenReturn(session);
			when(request.getSession(false)).thenReturn(session);
			when(request.getUserPrincipal()).thenReturn((Principal) null);
			when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
			when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
			for (Map.Entry<String, String[]> entry : parameters.entrySet()) {
				when(request.getParameter(entry.getKey())).thenReturn(entry.getValue()[0]);
				when(request.getParameterValues(entry.getKey())).thenReturn(entry.getValue());
			}
			return request;
		}

		private Enumeration<String> parameterNames() {
			return Collections.enumeration(List.copyOf(parameters.keySet()));
		}
	}

	private static final class CapturedResponse {
		private final StringWriter sink = new StringWriter();
		private final HttpServletResponse response = mock(HttpServletResponse.class);

		@SuppressWarnings("resource") // The servlet owns the response writer lifecycle.
		private CapturedResponse() throws IOException {
			when(response.getWriter()).thenReturn(new PrintWriter(sink, true));
		}

		private String body() {
			return sink.toString();
		}
	}
}
