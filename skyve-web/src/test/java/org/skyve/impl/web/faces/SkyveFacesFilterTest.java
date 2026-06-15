package org.skyve.impl.web.faces;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class SkyveFacesFilterTest {
	@AfterEach
	void tearDown() throws Exception {
		clearThreadPersistence();
	}

	@Test
	void doFilterDelegatesImmediatelyForNonFacesRequest() throws Exception {
		AbstractPersistence persistence = bindThreadPersistence();
		SkyveFacesFilter filter = new SkyveFacesFilter();
		filter.init(filterConfig(null, null, null, null, null));
		HttpServletRequest request = requestForPath("/images/logo.png");
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		verify(persistence).commit(true);
	}

	@Test
	void doFilterDelegatesImmediatelyForConfiguredUnsecuredFacesRequest() throws Exception {
		AbstractPersistence persistence = bindThreadPersistence();
		SkyveFacesFilter filter = new SkyveFacesFilter();
		filter.init(filterConfig("/login", "/error", "/expired", "/public", null));
		HttpServletRequest request = requestForPath("/public/page.jsf");
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		verify(persistence).commit(true);
	}

	@Test
	void initUsesConfiguredFacesSuffixWhenPresent() throws Exception {
		AbstractPersistence persistence = bindThreadPersistence();
		SkyveFacesFilter filter = new SkyveFacesFilter();
		filter.init(filterConfig("/login", "/error", "/expired", "/public", ".xhtml"));
		HttpServletRequest request = requestForPath("/public/page.xhtml");
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		verify(persistence).commit(true);
	}

	private static HttpServletRequest requestForPath(String servletPath) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getServletPath()).thenReturn(servletPath);
		return request;
	}

	private static FilterConfig filterConfig(String forward,
												String error,
												String expired,
												String unsecured,
												String facesSuffix) {
		ServletContext servletContext = mock(ServletContext.class);
		when(servletContext.getInitParameter("jakarta.faces.DEFAULT_SUFFIX")).thenReturn(facesSuffix);

		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("forward")).thenReturn(forward);
		when(config.getInitParameter("error")).thenReturn(error);
		when(config.getInitParameter("expired")).thenReturn(expired);
		when(config.getInitParameter("unsecured")).thenReturn(unsecured);
		when(config.getServletContext()).thenReturn(servletContext);
		return config;
	}

	private static AbstractPersistence bindThreadPersistence() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();
		return persistence;
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
