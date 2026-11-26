package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.RequestUxUiSelection;
import org.skyve.impl.web.UserAgent;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

@SuppressWarnings({ "resource", "java:S1989", "java:S5960" }) // Servlet exceptions and assertions are test-only.
class SmartClientSnapServletH2Test extends AbstractSkyveTest {
	/**
	 * A valid SmartClient snapshot of the contacts list grid.
	 */
	private static final String SNAPSHOT = "{\"criteria\":{\"_constructor\":\"AdvancedCriteria\",\"operator\":\"and\",\"criteria\":[{\"fieldName\":\"name\",\"operator\":\"iStartsWith\",\"value\":\"AA\"}]},\"advancedCriteriaStyle\":\"radio\",\"fieldState\":\"[{name:\\\"name\\\",width:246}]\",\"sortState\":\"({sortSpecifiers:[{property:\\\"name\\\",direction:\\\"ascending\\\"}]})\",\"groupState\":\"\",\"summaryType\":\"\"}";

	/**
	 * Build the test user's menus as login does; the menus are what grant list (aggregate) access.
	 */
	@BeforeEach
	void resetMenus() {
		ProvidedRepositoryFactory.getNullSafe().resetMenus(u);
	}

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
		verify(response.response).setHeader(org.mockito.ArgumentMatchers.eq("X-CSRF-TOKEN"), anyString());
	}

	@Test
	void doPostDelegatesToListSnapshotsForDocumentDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")).doPost();

		assertEquals("[]", response.body());
		verify(response.response).setHeader(org.mockito.ArgumentMatchers.eq("X-CSRF-TOKEN"), anyString());
	}

	@Test
	void doGetReturnsWarningForMalformedDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.param("a", "L")
				.param("d", "malformed")).doGet();

		assertTrue(response.body().contains("isc.warn('"), response.body());
		assertTrue(response.body().contains("Snapshot operation was unsuccessful"), response.body());
	}

	@Test
	void aNewSnapshotIsListedForItsDatasource() throws Exception {
		CapturedResponse created = service(newRequest(u)
				.withCsrfToken()
				.param("a", "N")
				.param("t", "sc")
				.param("d", "admin_qContacts")
				.param("n", "Starts with AA")
				.param("s", SNAPSHOT)).doGet();

		String body = created.body();
		assertTrue(body.startsWith("{\"bizId\":\""), body);
		String snapId = body.substring(10, body.length() - 2);
		// The servlet commits and closes the thread's persistence, so the next request starts another
		AbstractPersistence.get().setUser(u);

		CapturedResponse listed = service(newRequest(u)
				.param("a", "L")
				.param("t", "sc")
				.param("d", "admin_qContacts")).doGet();

		assertTrue(listed.body().startsWith("[{\"bizId\":\"" + snapId + "\",\"name\":\"Starts with AA\",\"snapshot\":{"), listed.body());
	}

	@Test
	void aNewSnapshotWithoutTheSessionsCsrfTokenIsRefusedAndNotCreated() throws Exception {
		CapturedResponse refused = service(newRequest(u)
				.param(AbstractWebContext.CSRF_TOKEN_NAME, StateUtil.createToken())
				.param("a", "N")
				.param("t", "sc")
				.param("d", "admin_qContacts")
				.param("n", "Refused")
				.param("s", SNAPSHOT)).doGet();

		assertUnexpectedWarning(refused.body());
		AbstractPersistence.get().setUser(u);
		CapturedResponse listed = service(newRequest(u)
				.param("a", "L")
				.param("t", "sc")
				.param("d", "admin_qContacts")).doGet();
		assertEquals("[]", listed.body());
	}

	@Test
	void aSnapshotDeleteWithoutTheSessionsCsrfTokenIsRefusedAndTheSnapshotStays() throws Exception {
		CapturedResponse created = service(newRequest(u)
				.withCsrfToken()
				.param("a", "N")
				.param("t", "sc")
				.param("d", "admin_qContacts")
				.param("n", "Kept")
				.param("s", SNAPSHOT)).doGet();
		String body = created.body();
		assertTrue(body.startsWith("{\"bizId\":\""), body);
		String snapId = body.substring(10, body.length() - 2);
		AbstractPersistence.get().setUser(u);

		CapturedResponse refused = service(newRequest(u)
				.param("a", "D")
				.param("i", snapId)).doGet();

		assertUnexpectedWarning(refused.body());
		AbstractPersistence.get().setUser(u);
		CapturedResponse listed = service(newRequest(u)
				.param("a", "L")
				.param("t", "sc")
				.param("d", "admin_qContacts")).doGet();
		assertTrue(listed.body().contains("\"bizId\":\"" + snapId + "\""), listed.body());
	}

	@Test
	void updateWithoutASnapshotIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "U")
				.param("i", "snap-1")).doGet();

		assertUnexpectedWarning(response.body());
	}

	@Test
	void newWithoutASnapshotNameIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "N")
				.param("d", "admin_qContacts")
				.param("s", SNAPSHOT)).doGet();

		assertUnexpectedWarning(response.body());
	}

	@Test
	void deleteWithoutASnapshotIdIsAnError() throws Exception {
		CapturedResponse response = service(newRequest(u)
				.withCsrfToken()
				.param("a", "D")).doGet();

		assertUnexpectedWarning(response.body());
	}

	/**
	 * Asserts the request failed unexpectedly, so the warning shows the generic message with a support reference.
	 */
	private static void assertUnexpectedWarning(String body) {
		assertTrue(body.startsWith("isc.warn('The Snapshot operation was unsuccessful."), body);
		assertTrue(body.contains(" Reference: "), body);
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
		// the session keeps its attributes, as a container's does, so its CSRF token stays the same
		private final Map<String, Object> attributes = new HashMap<>();
		private final HttpSession session = mock(HttpSession.class);

		private RequestBuilder(User user) {
			when(session.getId()).thenReturn("smart-client-snap-session-" + System.nanoTime());
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
			// as the desktop UX/UI, a UX/UI the test user has list access in
			when(request.getAttribute(UserAgent.class.getName() + ".selection")).thenReturn(
					new RequestUxUiSelection(UserAgentType.desktop,
												false,
												UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Cerulean", "omega")));
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
