package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.security.Principal;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

class DevLoginFilterTest {

	@Test
	@SuppressWarnings("static-method")
	void initSuccessAndDestroy() throws ServletException {
		DevLoginFilter filter = Assertions.assertDoesNotThrow(DevLoginFilter::new);
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("test");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");
		filter.init(config);
		filter.destroy();
	}

	@Test
	@SuppressWarnings("static-method")
	void initThrowsWhenCustomerMissing() {
		DevLoginFilter filter = new DevLoginFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn(null);
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	@SuppressWarnings("static-method")
	void initThrowsWhenUserMissing() {
		DevLoginFilter filter = new DevLoginFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("test");
		when(config.getInitParameter("user")).thenReturn(null);
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	@SuppressWarnings("static-method")
	void initThrowsWhenPasswordMissing() {
		DevLoginFilter filter = new DevLoginFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("test");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn(null);
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	@SuppressWarnings("static-method")
	void doFilterSkipsLoginWhenAlreadyAuthenticated() throws IOException, ServletException {
		DevLoginFilter filter = new DevLoginFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("test");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		Principal principal = mock(Principal.class);
		when(request.getUserPrincipal()).thenReturn(principal);
		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	@SuppressWarnings("static-method")
	void doFilterLogsInWhenNotAuthenticated() throws IOException, ServletException {
		DevLoginFilter filter = new DevLoginFilter();
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("test");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getUserPrincipal()).thenReturn(null);
		filter.doFilter(request, response, chain);
		verify(request).login("test/admin", "admin");
		verify(chain).doFilter(request, response);
	}
}
