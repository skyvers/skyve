package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.TestingAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.authentication.AuthenticationDetailsSource;
import org.springframework.security.web.AuthenticationEntryPoint;
import org.springframework.security.web.authentication.RememberMeServices;

import jakarta.servlet.FilterChain;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class SingleCustomerBasicAuthenticationFilterTest {
	private final String originalCustomer = UtilImpl.CUSTOMER;

	@AfterEach
	void cleanup() {
		UtilImpl.CUSTOMER = originalCustomer;
		SecurityContextHolder.clearContext();
	}

	@Test
	void skipsWhenAuthorizationHeaderMissing() throws Exception {
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(authenticationManager);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("Authorization")).thenReturn(null);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(authenticationManager, never()).authenticate(any());
		verify(chain).doFilter(request, response);
	}

	@Test
	void authenticatesAndStoresContextForValidBasicHeader() throws Exception {
		UtilImpl.CUSTOMER = "acme";
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(authenticationManager);
		RememberMeServices rememberMeServices = mock(RememberMeServices.class);
		filter.setRememberMeServices(rememberMeServices);
		AuthenticationDetailsSource<HttpServletRequest, Object> detailsSource = mock(AuthenticationDetailsSource.class);
		Object details = new Object();
		when(detailsSource.buildDetails(any(HttpServletRequest.class))).thenReturn(details);
		filter.setAuthenticationDetailsSource(detailsSource);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("Authorization")).thenReturn("Basic " + base64("alice:secret"));
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		Authentication authResult = new TestingAuthenticationToken("acme/alice", "secret", "ROLE_USER");
		when(authenticationManager.authenticate(any(Authentication.class))).thenReturn(authResult);

		filter.doFilter(request, response, chain);

		Authentication stored = SecurityContextHolder.getContext().getAuthentication();
		assertTrue(stored.isAuthenticated());
		assertEquals("acme/alice", stored.getName());
		verify(rememberMeServices).loginSuccess(request, response, authResult);
		verify(chain).doFilter(request, response);
	}

	@Test
	void invokesEntryPointOnAuthenticationFailureWhenConfigured() throws Exception {
		UtilImpl.CUSTOMER = "acme";
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		AuthenticationEntryPoint entryPoint = mock(AuthenticationEntryPoint.class);
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(authenticationManager, entryPoint);
		RememberMeServices rememberMeServices = mock(RememberMeServices.class);
		filter.setRememberMeServices(rememberMeServices);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("Authorization")).thenReturn("Basic " + base64("alice:bad"));
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		BadCredentialsException failure = new BadCredentialsException("bad credentials");
		when(authenticationManager.authenticate(any(Authentication.class))).thenThrow(failure);

		filter.doFilter(request, response, chain);

		verify(rememberMeServices).loginFail(request, response);
		verify(entryPoint).commence(request, response, failure);
		verify(chain, never()).doFilter(any(), any());
		assertNull(SecurityContextHolder.getContext().getAuthentication());
	}

	@Test
	void invalidBase64HeaderFallsThroughWhenIgnoringFailure() throws Exception {
		AuthenticationManager authenticationManager = mock(AuthenticationManager.class);
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(authenticationManager);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getHeader("Authorization")).thenReturn("Basic !!!notbase64!!!");
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
	}

	@Test
	void extractAndDecodeHeaderPrefixesCustomerName() throws Exception {
		UtilImpl.CUSTOMER = "tenant";
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(mock(AuthenticationManager.class));
		HttpServletRequest request = mock(HttpServletRequest.class);
		String[] tokens = invokeExtractAndDecodeHeader(filter, "Basic " + base64("bob:pwd"), request);

		assertArrayEquals(new String[] {"tenant/bob", "pwd"}, tokens);
	}

	@Test
	void extractAndDecodeHeaderRejectsTokenWithoutDelimiter() {
		SingleCustomerBasicAuthenticationFilter filter = new SingleCustomerBasicAuthenticationFilter(mock(AuthenticationManager.class));
		HttpServletRequest request = mock(HttpServletRequest.class);
		String header = "Basic " + base64("nodelimiter");

		assertThrows(BadCredentialsException.class,
				() -> invokeExtractAndDecodeHeader(filter, header, request));
	}

	@Test
	void authenticationRequiredRecognisesAnonymousAndDifferentUsers() throws Exception {
		SecurityContextHolder.getContext().setAuthentication(null);
		assertTrue(invokeAuthenticationRequired("acme/alice"));

		UsernamePasswordAuthenticationToken sameUser = new UsernamePasswordAuthenticationToken("acme/alice",
				"x",
				AuthorityUtils.createAuthorityList("ROLE_USER"));
		SecurityContextHolder.getContext().setAuthentication(sameUser);
		assertFalse(invokeAuthenticationRequired("acme/alice"));

		UsernamePasswordAuthenticationToken differentUser = new UsernamePasswordAuthenticationToken("acme/bob",
				"x",
				AuthorityUtils.createAuthorityList("ROLE_USER"));
		SecurityContextHolder.getContext().setAuthentication(differentUser);
		assertTrue(invokeAuthenticationRequired("acme/alice"));

		AnonymousAuthenticationToken anonymous = new AnonymousAuthenticationToken("key", "anonymousUser", AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS"));
		SecurityContextHolder.getContext().setAuthentication(anonymous);
		assertTrue(invokeAuthenticationRequired("acme/alice"));
	}

	private static String base64(String plain) {
		return Base64.getEncoder().encodeToString(plain.getBytes(StandardCharsets.UTF_8));
	}

	private static String[] invokeExtractAndDecodeHeader(SingleCustomerBasicAuthenticationFilter filter,
														String header,
														HttpServletRequest request) throws Exception {
		Method method = SingleCustomerBasicAuthenticationFilter.class.getDeclaredMethod("extractAndDecodeHeader", String.class, HttpServletRequest.class);
		method.setAccessible(true);
		try {
			return (String[]) method.invoke(filter, header, request);
		}
		catch (java.lang.reflect.InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof IOException io) {
				throw io;
			}
			if (cause instanceof RuntimeException runtimeException) {
				throw runtimeException;
			}
			if (cause instanceof Error error) {
				throw error;
			}
			throw new RuntimeException(cause);
		}
	}

	private static boolean invokeAuthenticationRequired(String username) throws Exception {
		Method method = SingleCustomerBasicAuthenticationFilter.class.getDeclaredMethod("authenticationRequired", String.class);
		method.setAccessible(true);
		Object result = method.invoke(null, username);
		return ((Boolean) result).booleanValue();
	}
}
