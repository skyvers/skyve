package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
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

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

@SuppressWarnings({ "resource", "java:S1989", "java:S5960" }) // Servlet exceptions and assertions are test-only.
class SmartClientSnapServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doGetListsSnapshotsForDocumentDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")
				.param("t", "sc")).doGet();

		assertEquals("[]", response.body());
		verify(response.response).setIntHeader(org.mockito.ArgumentMatchers.eq("X-CSRF-TOKEN"), anyInt());
	}

	@Test
	void doPostDelegatesToListSnapshotsForDocumentDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")).doPost();

		assertEquals("[]", response.body());
		verify(response.response).setIntHeader(org.mockito.ArgumentMatchers.eq("X-CSRF-TOKEN"), anyInt());
	}

	@Test
	void doGetReturnsWarningForMalformedDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")
				.param("d", "malformed")).doGet();

		assertTrue(response.body().contains("isc.warn('"), response.body());
		assertTrue(response.body().contains("Snapshot operation was unsuccessful"), response.body());
	}

	private static ServletHarness service(RequestBuilder request) throws IOException {
		CapturedResponse response = new CapturedResponse();
		return new ServletHarness(request.build(), response);
	}

	private static RequestBuilder newRequest(User user) {
		return new RequestBuilder(user);
	}

	private static final class ServletHarness {
		private final TestableSmartClientSnapServlet servlet = new TestableSmartClientSnapServlet();
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

	private static final class TestableSmartClientSnapServlet extends SmartClientSnapServlet {
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
			when(session.getId()).thenReturn("smart-client-snap-session-" + System.nanoTime());
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

		private CapturedResponse() throws IOException {
			when(response.getWriter()).thenReturn(new PrintWriter(sink, true));
		}

		private String body() {
			return sink.toString();
		}
	}
}
