package org.skyve.impl.web.filter.rest;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.app.AppConstants;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;

@SuppressWarnings({"static-method", "resource", "null"})
class BasicAuthFilterTest {

	private static class CapturingOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream buf = new ByteArrayOutputStream();

		@Override
		public void write(int b) {
			buf.write(b);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener l) {
			// no-op
		}

	}

	@Test
	void noAuthorizationHeaderReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn(null);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
	}

	@Test
	void nonBasicAuthorizationHeaderReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Bearer sometoken");
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
	}

	@Test
	void emptyUsernameInCredentialsReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		// Credentials ":password" => username is empty string => null after processStringValue
		String credentials = Base64.getMimeEncoder().encodeToString(":password".getBytes(StandardCharsets.UTF_8));
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
	}

	@Test
	void emptyPasswordInCredentialsReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		// Credentials "username:" => password is empty string => null after processStringValue
		String credentials = Base64.getMimeEncoder().encodeToString("username:".getBytes(StandardCharsets.UTF_8));
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
	}

	@Test
	void unsecuredPrefixBypassesAuthentication() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("unsecured")).thenReturn("/api/public");
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/api/public/health");

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		verify(request, never()).getHeader("Authorization");
	}

	@Test
	void malformedBase64AuthorizationThrowsIllegalArgumentException() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic !!!not-base64!!!");

		Assertions.assertThrows(IllegalArgumentException.class,
				() -> filter.doFilter(request, response, chain));
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void credentialsWithoutDelimiterThrowsArrayIndexOutOfBoundsException() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		String credentials = Base64.getMimeEncoder().encodeToString("usernameOnly".getBytes(StandardCharsets.UTF_8));
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);

		Assertions.assertThrows(ArrayIndexOutOfBoundsException.class,
				() -> filter.doFilter(request, response, chain));
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void validCredentialsCallsChain() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = (UtilImpl.CUSTOMER == null) ? "cust/user" : "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);
		when(sql.putParameter(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(EXT.hashPassword(password));

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);

			filter.doFilter(request, response, chain);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(chain).doFilter(request, response);
		verify(persistence).begin();
		verify(persistence).setUser(user);
		verify(persistence, never()).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void missingUserReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);

		String username = "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);
		when(repository.retrieveUser(username)).thenReturn(null);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);

			filter.doFilter(request, response, chain);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
		verify(persistence).begin();
		verify(persistence).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void metadataExceptionDuringCredentialValidationReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);
		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenThrow(new MetaDataException("boom"));

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);

			filter.doFilter(request, response, chain);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
		verify(persistence).begin();
		verify(persistence).setUser(user);
		verify(persistence, org.mockito.Mockito.times(2)).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void unexpectedExceptionFromFilterChainReturnsGenericError() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = (UtilImpl.CUSTOMER == null) ? "cust/user" : "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);
		when(sql.putParameter(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(EXT.hashPassword(password));
		org.mockito.Mockito.doThrow(new RuntimeException("boom")).when(chain).doFilter(request, response);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);

			filter.doFilter(request, response, chain);
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(persistence, org.mockito.Mockito.times(2)).rollback();
		verify(persistence).commit(true);
	}

	@Test
	void multiTenantUsernameWithoutCustomerReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		String previousCustomer = UtilImpl.CUSTOMER;
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);
			UtilImpl.CUSTOMER = null;

			filter.doFilter(request, response, chain);
		}
		finally {
			UtilImpl.CUSTOMER = previousCustomer;
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void singleTenantUsernameWithoutCustomerValidatesPassword() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);
		when(sql.putParameter(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(EXT.hashPassword(password));

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		String previousCustomer = UtilImpl.CUSTOMER;
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);
			UtilImpl.CUSTOMER = "cust";

			filter.doFilter(request, response, chain);
		}
		finally {
			UtilImpl.CUSTOMER = previousCustomer;
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(sql).putParameter(AppConstants.USER_NAME_ATTRIBUTE_NAME, username, false);
		verify(chain).doFilter(request, response);
	}

	@Test
	void singleTenantCustomerPrefixedUsernameReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "cust/user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		String previousCustomer = UtilImpl.CUSTOMER;
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);
			UtilImpl.CUSTOMER = "cust";

			filter.doFilter(request, response, chain);
		}
		finally {
			UtilImpl.CUSTOMER = previousCustomer;
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void nullHashedPasswordReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "cust/user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);
		when(sql.putParameter(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(null);

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		String previousCustomer = UtilImpl.CUSTOMER;
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);
			UtilImpl.CUSTOMER = null;

			filter.doFilter(request, response, chain);
		}
		finally {
			UtilImpl.CUSTOMER = previousCustomer;
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void passwordMismatchReturnsForbidden() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Module adminModule = mock(Module.class, RETURNS_DEEP_STUBS);
		SQL sql = mock(SQL.class);
		UserImpl user = new UserImpl();
		user.setName("user");

		String username = "cust/user";
		String password = "secret";
		String credentials = Base64.getMimeEncoder().encodeToString((username + ':' + password).getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		when(repository.retrieveUser(username)).thenReturn(user);
		when(repository.getModule(null, AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(null, AppConstants.USER_DOCUMENT_NAME).getPersistent().getPersistentIdentifier()).thenReturn("ADM_SecurityUser");
		when(persistence.newSQL(org.mockito.ArgumentMatchers.anyString())).thenReturn(sql);
		when(sql.putParameter(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(EXT.hashPassword("different"));

		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ThreadLocal<AbstractPersistence> threadLocal = getPersistenceThreadLocal();
		AbstractPersistence previousPersistence = threadLocal.get();
		String previousCustomer = UtilImpl.CUSTOMER;
		try {
			ProvidedRepositoryFactory.set(repository);
			threadLocal.set(persistence);
			UtilImpl.CUSTOMER = null;

			filter.doFilter(request, response, chain);
		}
		finally {
			UtilImpl.CUSTOMER = previousCustomer;
			ProvidedRepositoryFactory.set(previousRepository);
			if (previousPersistence != null) {
				threadLocal.set(previousPersistence);
			}
			else {
				threadLocal.remove();
			}
		}

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void handleAuthFailureRethrowsError() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		Method method = BasicAuthFilter.class.getDeclaredMethod("handleAuthFailure",
				AbstractPersistence.class,
				HttpServletRequest.class,
				HttpServletResponse.class,
				Throwable.class);
		method.setAccessible(true);

		AssertionError boom = new AssertionError("boom");
		InvocationTargetException ex = Assertions.assertThrows(InvocationTargetException.class,
				() -> method.invoke(filter, null, null, null, boom));
		Assertions.assertSame(boom, ex.getCause());
	}

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> getPersistenceThreadLocal() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		return (ThreadLocal<AbstractPersistence>) field.get(null);
	}
}
