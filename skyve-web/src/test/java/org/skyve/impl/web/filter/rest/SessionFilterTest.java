package org.skyve.impl.web.filter.rest;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;

@SuppressWarnings({"static-method", "resource"})
class SessionFilterTest {
	private static final String USER_ID = "apiUserId";

	private final ProvidedRepository originalRepository = readProvidedRepository();
	private boolean originalForceNonPersistentCaching;
	private String originalCacheDirectory;
	private SessionCacheConfig originalSessionCache;
	private Path cacheDirectory;

	@BeforeEach
	void setUp() throws Exception {
		originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		originalSessionCache = UtilImpl.SESSION_CACHE;

		cacheDirectory = Files.createTempDirectory("session-filter-cache");
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
		UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
		UtilImpl.SESSION_CACHE = new SessionCacheConfig(100, 10);

		DefaultCaching.get().shutdown();
		DefaultCaching.get().startup();
		StateUtil.removeSessions(USER_ID);
	}

	@AfterEach
	void tearDown() throws Exception {
		StateUtil.removeSessions(USER_ID);
		DefaultCaching.get().shutdown();
		setProvidedRepository(originalRepository);
		unbindPersistenceFromThread();
		UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
		UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
		UtilImpl.SESSION_CACHE = originalSessionCache;

		if (cacheDirectory != null) {
			try (Stream<Path> paths = Files.walk(cacheDirectory)) {
				paths.sorted((a, b) -> b.compareTo(a))
					.map(Path::toFile)
					.forEach(File::delete);
			}
		}
	}

	@Test
	void doFilterPassesThroughWhenUrlIsConfiguredAsUnsecured() throws Exception {
		SessionFilter filter = new SessionFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("unsecured")).thenReturn("/api/public");
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/api/public/health");

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
	}

	@Test
	void doFilterSendsUnauthorizedWhenNoSessionUserOrPrincipalExists() throws Exception {
		SessionFilter filter = new SessionFilter();
		filter.init(mock(FilterConfig.class));

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getSession(false)).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getHeader("Authorization")).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);

		filter.doFilter(request, response, chain);

		verify(persistence).evictAllCached();
		verify(persistence).begin();
		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(chain, never()).doFilter(request, response);
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void doFilterSetsUserAndDelegatesWhenSessionUserIsResolved() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = mock(UserImpl.class);
		when(user.getId()).thenReturn("apiUserId");
		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getCustomer()).thenReturn(null);
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		setProvidedRepository(repository);

		SessionFilter filter = new SessionFilter();
		filter.init(mock(FilterConfig.class));

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);
		jakarta.servlet.http.HttpSession session = mock(jakarta.servlet.http.HttpSession.class);
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-rest-1");
		StateUtil.addSession(USER_ID, session);

		filter.doFilter(request, response, chain);

		verify(persistence, atLeastOnce()).setUser(user);
		verify(chain).doFilter(request, response);
		verify(persistence, never()).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void doFilterRollsBackAndReturnsGenericErrorWhenDownstreamThrowsForSessionUser() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = mock(UserImpl.class);
		when(user.getId()).thenReturn("apiUserId");
		when(user.getName()).thenReturn("apiUser");
		when(user.getCustomerName()).thenReturn("demo");
		when(user.getCustomer()).thenReturn(null);
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		setProvidedRepository(repository);

		SessionFilter filter = new SessionFilter();
		filter.init(mock(FilterConfig.class));

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);
		jakarta.servlet.http.HttpSession session = mock(jakarta.servlet.http.HttpSession.class);
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getSession(false)).thenReturn(session);
		when(session.getAttribute(org.skyve.web.WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		when(request.getUserPrincipal()).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(session.getId()).thenReturn("session-rest-2");
		StateUtil.addSession(USER_ID, session);
		org.mockito.Mockito.doThrow(new ServletException("boom")).when(chain).doFilter(request, response);

		filter.doFilter(request, response, chain);

		verify(persistence, org.mockito.Mockito.times(2)).rollback();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(persistence).commit(true);
	}

	@Test
	void doFilterCoversPrincipalNameBranchWhenPrincipalUserCannotBeResolved() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrieveUser("principalUser")).thenReturn(null);
		setProvidedRepository(repository);

		SessionFilter filter = new SessionFilter();
		filter.init(mock(FilterConfig.class));

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);
		java.security.Principal principal = mock(java.security.Principal.class);
		when(principal.getName()).thenReturn("principalUser");
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getSession(false)).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn(principal);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);

		filter.doFilter(request, response, chain);

		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(persistence, org.mockito.Mockito.times(2)).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void doFilterHandlesPersistenceCreationFailureWithNullPersistence() throws Exception {
		SessionFilter filter = new SessionFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getLocale()).thenReturn(Locale.ENGLISH);

		Class<? extends AbstractPersistence> previousImplementation = AbstractPersistence.IMPLEMENTATION_CLASS;
		try {
			unbindPersistenceFromThread();
			AbstractPersistence.IMPLEMENTATION_CLASS = null;

			filter.doFilter(request, response, chain);
		}
		finally {
			AbstractPersistence.IMPLEMENTATION_CLASS = previousImplementation;
		}

		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}

	private static HttpServletResponse mockResponse() throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);
		when(response.getOutputStream()).thenReturn(new BufferingServletOutputStream());
		return response;
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

	private static ProvidedRepository readProvidedRepository() {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			return (ProvidedRepository) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Could not read repository field", e);
		}
	}

	private static void setProvidedRepository(ProvidedRepository repository) {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			field.set(null, repository);
		}
		catch (ReflectiveOperationException e) {
			throw new IllegalStateException("Could not set repository field", e);
		}
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
