package org.skyve.impl.web.filter.rest;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import jakarta.ws.rs.core.MediaType;

@SuppressWarnings({"static-method", "resource"})
class RestUserPersistenceFilterTest {
	private final ProvidedRepository originalRepository = readProvidedRepository();

	@AfterEach
	void tearDown() throws Exception {
		setProvidedRepository(originalRepository);
		unbindPersistenceFromThread();
	}

	@Test
	void doFilterSetsPersistenceUserAndDelegatesWhenRepositoryResolvesUser() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = new UserImpl();
		when(repository.retrieveUser("apiUser")).thenReturn(user);
		setProvidedRepository(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession(false)).thenReturn(session);
		when(session.getId()).thenReturn("session-1");
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);

		RestUserPersistenceFilter filter = new RestUserPersistenceFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("PersistenceUser")).thenReturn("apiUser");
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(persistence).evictAllCached();
		verify(persistence).begin();
		verify(persistence).setUser(user);
		verify(chain).doFilter(request, response);
		verify(persistence).commit(true);
	}

	@Test
	void doFilterSendsForbiddenWhenConfiguredPersistenceUserCannotBeResolved() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.retrieveUser("missing")).thenReturn(null);
		setProvidedRepository(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);

		RestUserPersistenceFilter filter = new RestUserPersistenceFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("PersistenceUser")).thenReturn("missing");
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
		verify(persistence).commit(true);
	}

	@Test
	void doFilterSendsForbiddenWhenSecurityExceptionOccurs() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		doThrow(new SecurityException("denied")).when(persistence).begin();
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);

		RestUserPersistenceFilter filter = new RestUserPersistenceFilter();
		filter.init(mock(FilterConfig.class));
		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(persistence, atLeastOnce()).rollback();
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void doFilterSendsForbiddenWhenMetaDataExceptionOccurs() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		doThrow(new MetaDataException("metadata")).when(persistence).begin();
		bindPersistenceToThread(persistence);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mockResponse();
		FilterChain chain = mock(FilterChain.class);

		RestUserPersistenceFilter filter = new RestUserPersistenceFilter();
		filter.init(mock(FilterConfig.class));
		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(persistence, atLeastOnce()).rollback();
		verify(chain, never()).doFilter(request, response);
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
