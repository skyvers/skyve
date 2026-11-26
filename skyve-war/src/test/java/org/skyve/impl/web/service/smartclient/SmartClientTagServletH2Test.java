package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.Principal;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

@SuppressWarnings("resource")
class SmartClientTagServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doGetListsTagMenuActions() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")
				.param("ID", "tagButton")).doGet();

		assertBaseTagMenu(response.body());
		verify(response.response).setHeader(eq("X-CSRF-TOKEN"), anyString());
	}

	@Test
	void doPostDelegatesToListTagMenuActions() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")
				.param("ID", "tagButton")).doPost();

		assertBaseTagMenu(response.body());
		verify(response.response).setHeader(eq("X-CSRF-TOKEN"), anyString());
	}

	@Test
	void doGetReturnsWarningForMutatingActionWithoutValidCsrfToken() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "T")
				.param("t", "missing-tag")
				.param("d", "malformed")).doGet();

		assertTrue(response.body().contains("isc.warn('"), response.body());
		assertTrue(response.body().contains("tag operation was unsuccessful"), response.body());
	}

	@Test
	@SuppressWarnings("java:S5976") // each tag action's missing-parameter error reads better as its own named test
	void listWithoutAMenuButtonIdIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")).doGet();

		assertUnexpectedWarning(response.body());
	}

	@Test
	void untagWithoutCriteriaIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "U")
				.param("t", "tag-1")
				.param("d", "admin_qContacts")).doGet();

		assertUnexpectedWarning(response.body());
	}

	@Test
	void clearWithoutATagIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "C")).doGet();

		assertUnexpectedWarning(response.body());
	}

	@Test
	void aNewTagWithoutTheSessionsCsrfTokenIsRefusedAndNotCreated() throws Exception {
		String name = "refused-tag-" + System.nanoTime();
		CapturedResponse refused = service(newRequest(u)
				.param(AbstractWebContext.CSRF_TOKEN_NAME, StateUtil.createToken())
				.param("a", "N")
				.param("n", name)).doGet();

		assertUnexpectedWarning(refused.body());
		AbstractPersistence.get().setUser(u);
		assertNull(EXT.getTagManager().getTagId(name));
	}

	@Test
	void newTagWithoutANameIsAnErrorAndCreatesNoTag() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "N")).doGet();

		assertUnexpectedWarning(response.body());
		assertFalse(response.body().contains("{bizId:'"), response.body());
	}

	@Test
	void deleteWithoutATagIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "D")).doGet();

		assertUnexpectedWarning(response.body());
	}

	/**
	 * Asserts the request failed unexpectedly, so the warning shows the generic message with a support reference.
	 */
	private static void assertUnexpectedWarning(String body) {
		assertTrue(body.startsWith("isc.warn('The tag operation was unsuccessful."), body);
		assertTrue(body.contains(" Reference: "), body);
	}

	private static void assertBaseTagMenu(String body) {
		assertTrue(body.contains("New Tag"), body);
		assertTrue(body.contains("No Tag"), body);
		assertTrue(body.contains("tagButton.newTag()"), body);
		assertTrue(body.contains("tagButton.setTag(null,'No Tag')"), body);
	}

	private static ServletHarness service(RequestBuilder request) throws IOException {
		CapturedResponse response = new CapturedResponse();
		return new ServletHarness(request.build(), response);
	}

	private static RequestBuilder newRequest(User user) {
		return new RequestBuilder(user);
	}

	private static final class ServletHarness {
		private final TestableSmartClientTagServlet servlet = new TestableSmartClientTagServlet();
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

	private static final class TestableSmartClientTagServlet extends SmartClientTagServlet {
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
		// the session keeps its attributes, as a container's does, so its CSRF token stays the same
		private final Map<String, Object> attributes = new HashMap<>();
		private final HttpSession session = mock(HttpSession.class);

		private RequestBuilder(User user) {
			when(session.getId()).thenReturn("smart-client-tag-session-" + System.nanoTime());
			attributes.put(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
			when(session.getAttribute(anyString())).thenAnswer(i -> attributes.get(i.getArgument(0)));
			doAnswer(i -> attributes.put(i.getArgument(0), i.getArgument(1))).when(session).setAttribute(anyString(), any());
			StateUtil.addSession(user.getId(), session);
		}

		private RequestBuilder param(String name, String value) {
			parameters.put(name, new String[] { value });
			return this;
		}

		/**
		 * Sends the session's CSRF token, as the client does with a mutating request.
		 */
		private RequestBuilder withCsrfToken() {
			return param(AbstractWebContext.CSRF_TOKEN_NAME, StateUtil.getSessionToken(session));
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
