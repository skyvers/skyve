package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.Principal;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.user.User;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

class SmartClientCompleteServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doGetReturnsSuccessfulSuggestResponseForPersistentTextAttribute() throws Exception {
		AllAttributesPersistent context = new DataBuilder().fixture(FixtureType.crud)
															.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		RequestBuilder request = newRequest(u)
				.withConversation(context)
				.param(AbstractWebContext.CONTEXT_NAME, null)
				.param(AbstractWebContext.ACTION_NAME, CompleteType.suggest.toString())
				.param("_attr", "text")
				.param("value", "complete-suggest")
				.param("_startRow", "0")
				.param("_endRow", "10");
		request.param(AbstractWebContext.CONTEXT_NAME, request.webId());

		CapturedResponse response = service(request).doGet();

		assertSuccessfulResponse(response.body());
		verifyJsonResponseHeaders(response.response);
	}

	@Test
	void doGetReturnsSmartClientErrorWhenAttributeIsMissing() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param(AbstractWebContext.CONTEXT_NAME, "missing-context")
				.param(AbstractWebContext.ACTION_NAME, CompleteType.previous.toString())).doGet();

		assertErrorResponse(response.body());
		verifyJsonResponseHeaders(response.response);
	}

	@Test
	void doPostDelegatesToSmartClientErrorWhenConversationIsMissing() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("_attr", "text")
				.param(AbstractWebContext.ACTION_NAME, CompleteType.previous.toString())).doPost();

		assertErrorResponse(response.body());
		verifyJsonResponseHeaders(response.response);
	}

	private static void assertErrorResponse(String body) {
		assertTrue(body.contains("\"status\":-1"), body);
		assertTrue(body.contains("\"totalRows\":0"), body);
	}

	private static void assertSuccessfulResponse(String body) {
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
	}

	private static void verifyJsonResponseHeaders(HttpServletResponse response) {
		verify(response).setContentType(MimeType.json.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
		verify(response).addHeader("Cache-control", "private,no-cache,no-store");
		verify(response).addDateHeader("Expires", 0);
	}

	private static ServletHarness service(RequestBuilder request) throws IOException {
		CapturedResponse response = new CapturedResponse();
		return new ServletHarness(request.build(), response);
	}

	private static RequestBuilder newRequest(User user) {
		return new RequestBuilder(user);
	}

	private static final class ServletHarness {
		private final TestableSmartClientCompleteServlet servlet = new TestableSmartClientCompleteServlet();
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

	private static final class TestableSmartClientCompleteServlet extends SmartClientCompleteServlet {
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
		private String webId;

		private RequestBuilder(User user) {
			when(session.getId()).thenReturn("smart-client-complete-session-" + System.nanoTime());
			when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
			StateUtil.addSession(user.getId(), session);
		}

		private RequestBuilder withConversation(AllAttributesPersistent bean) throws Exception {
			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getSession(false)).thenReturn(session);
			when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
			SmartClientWebContext webContext = new SmartClientWebContext(UUID.randomUUID().toString(), request);
			webContext.setCurrentBean(bean);
			StateUtil.cacheConversation(webContext);
			webId = webContext.getWebId();
			return this;
		}

		private String webId() {
			return webId;
		}

		private RequestBuilder param(String name, String value) {
			parameters.put(name, new String[] {value});
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
			when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(null);
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

		@SuppressWarnings("resource")
		private CapturedResponse() throws IOException {
			when(response.getWriter()).thenReturn(new PrintWriter(sink, true));
		}

		private String body() {
			return sink.toString();
		}
	}
}
