package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.RETURNS_SELF;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.security.Principal;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.web.WebContext;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "resource", "java:S5976"})
class SmartClientCompleteServletTest {

	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() throws Exception {
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void doGetWarnsWhenAttributeIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn(null);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenConversationIdIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doPostWarnsWhenAttributeIsMissing() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn(null);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doPost(request, response));
	}

	@Test
	void doGetWarnsWhenUserCannotBeResolved() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithoutUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenActionIsInvalid() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-1");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("notACompleteType");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	@Test
	void doGetWarnsWhenConversationCannotBeResolved() throws Exception {
		SmartClientCompleteServlet servlet = new SmartClientCompleteServlet();
		HttpServletRequest request = newRequestWithSessionUser();
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getWriter()).thenReturn(mock(PrintWriter.class, RETURNS_SELF));
		when(request.getParameter("_attr")).thenReturn("name");
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx-missing");
		when(request.getParameter(AbstractWebContext.ACTION_NAME)).thenReturn("previous");

		assertDoesNotThrow(() -> servlet.doGet(request, response));
	}

	private HttpServletRequest newRequestWithSessionUser() {
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
		when(request.getSession(false)).thenReturn(session);
		when(request.getSession(true)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(principal);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-1");
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(null);
		return request;
	}

	private static HttpServletRequest newRequestWithoutUser() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(request.getSession(true)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
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