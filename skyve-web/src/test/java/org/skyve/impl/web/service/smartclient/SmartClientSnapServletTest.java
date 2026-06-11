package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.security.Principal;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "resource"})
class SmartClientSnapServletTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() throws Exception {
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void doGetWarnsWhenUserCannotBeResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithoutUser("L");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doPostWarnsWhenUserCannotBeResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithoutUser("L");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));

		assertDoesNotThrow(() -> servlet.doPost(request, response));
	}

	@Test
	void doGetWarnsWhenActionIsMissing() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithoutUser(null);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenListActionHasExistingCsrfToken() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithoutUser("L");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetHandlesUnknownActionWhenUserIsResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithSessionUser("X");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doPostHandlesUnknownActionWhenUserIsResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithSessionUser("X");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");

		assertDoesNotThrow(() -> servlet.doPost(request, response));
	}

	@Test
	void doGetHandlesListActionWhenUserIsResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithSessionUser("L");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");
		when(request.getParameter("t")).thenReturn("sc");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doPostHandlesListActionWhenUserIsResolved() throws Exception {
		SmartClientSnapServlet servlet = new SmartClientSnapServlet();
		HttpServletRequest request = newRequestWithSessionUser("L");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, Mockito.RETURNS_SELF));
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");
		when(request.getParameter("t")).thenReturn("sc");

		assertDoesNotThrow(() -> servlet.doPost(request, response));
	}

	@Test
	void mutatingActionsRenderWarningWhenRuntimeStateIsUnavailable() throws Exception {
		for (String action : new String[] {"U", "N", "D"}) {
			SmartClientSnapServlet servlet = new SmartClientSnapServlet();
			HttpServletRequest request = newRequestWithSessionUser(action);
			StringWriter sink = new StringWriter();
			HttpServletResponse response = mock(HttpServletResponse.class);
			when(response.getWriter()).thenReturn(new PrintWriter(sink));
			when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn("7");
			when(request.getParameter("i")).thenReturn("snap-1");
			when(request.getParameter("n")).thenReturn("Snapshot");
			when(request.getParameter("s")).thenReturn("{}");

			assertDoesNotThrow(() -> servlet.doGet(request, response));

			assertTrue(sink.toString().contains("The Snapshot operation was unsuccessful."));
		}
	}

	@Test
	void appendUnexpectedWarningIncludesFailurePrefix() {
		StringWriter sink = new StringWriter();
		PrintWriter writer = new PrintWriter(sink);

		SmartClientSnapServlet.appendUnexpectedWarning("REF-32", writer);
		writer.flush();

		assertTrue(sink.toString().contains("The Snapshot operation was unsuccessful."));
	}

	private static HttpServletRequest newRequestWithoutUser(String action) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getParameter("a")).thenReturn(action);
		when(request.getParameter(org.skyve.impl.web.AbstractWebContext.CSRF_TOKEN_NAME)).thenReturn(null);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		return request;
	}

	private static HttpServletRequest newRequestWithSessionUser(String action) {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = new UserImpl();
		user.setId("apiUserId");
		user.setCustomerName("demo");
		user.setName("apiUser");
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		Principal principal = mock(Principal.class);
		when(principal.getName()).thenReturn("apiUser");
		when(request.getParameter("a")).thenReturn(action);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getSession(true)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(principal);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-snap-38");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		return request;
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
