package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;
import java.security.Principal;
import java.util.Collections;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "boxing"})
class SmartClientTextSearchServletTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void tearDown() throws Exception {
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadPersistence();
	}

	@Test
	void doGetWrapsInfrastructureFailuresAsServletException() {
		SmartClientTextSearchServlet servlet = new SmartClientTextSearchServlet();
		HttpServletRequest request = org.mockito.Mockito.mock(HttpServletRequest.class);
		HttpServletResponse response = org.mockito.Mockito.mock(HttpServletResponse.class);

		when(request.getParameterNames()).thenReturn(Collections.enumeration(Collections.singleton("query")));
		when(request.getParameter("query")).thenReturn("smith");

		assertThrows(ServletException.class, () -> servlet.doGet(request, response));
	}

	@Test
	void doGetHandlesRequestsWithoutQueryParameter() {
		SmartClientTextSearchServlet servlet = new SmartClientTextSearchServlet();
		HttpServletRequest request = org.mockito.Mockito.mock(HttpServletRequest.class);
		HttpServletResponse response = org.mockito.Mockito.mock(HttpServletResponse.class);

		when(request.getParameterNames()).thenReturn(Collections.emptyEnumeration());

		assertThrows(ServletException.class, () -> servlet.doGet(request, response));
	}

	@Test
	void doGetWrapsMissingUserPathAsServletExceptionWhenPersistenceIsBound() {
		bindThreadPersistence();

		SmartClientTextSearchServlet servlet = new SmartClientTextSearchServlet();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);

		when(request.getParameterNames()).thenReturn(Collections.enumeration(Collections.singleton("query")));
		when(request.getParameter("query")).thenReturn("smith");
		when(request.getSession(false)).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);

		assertThrows(ServletException.class, () -> servlet.doGet(request, response));
	}

	@Test
	void doGetWrapsSecurityPathAsServletExceptionWhenUserCannotTextSearch() {
		bindThreadPersistence();

		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = mock(UserImpl.class);
		when(user.canTextSearch()).thenReturn(false);
		when(user.getName()).thenReturn("apiUser");
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		ProvidedRepositoryFactory.set(repository);

		SmartClientTextSearchServlet servlet = new SmartClientTextSearchServlet();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		HttpSession session = mock(HttpSession.class);
		Principal principal = mock(Principal.class);

		when(principal.getName()).thenReturn("apiUser");
		when(request.getUserPrincipal()).thenReturn(principal);
		when(request.getSession(false)).thenReturn(null);
		when(request.getSession(true)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(request.getParameterNames()).thenReturn(Collections.enumeration(Collections.singleton("query")));
		when(request.getParameter("query")).thenReturn("smith");
		when(session.getId()).thenReturn("session-36");

		assertThrows(ServletException.class, () -> servlet.doGet(request, response));
	}

	private static void bindThreadPersistence() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
