package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.WebContainer;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class RequestLoggingAndStatisticsFilterTest {
	@Test
	void doFilterSkipsStaticUrlPrefixes() throws Exception {
		RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
		FilterConfig config = mock(FilterConfig.class);
		ServletContext servletContext = mock(ServletContext.class);
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(config.getServletContext()).thenReturn(servletContext);
		when(servletContext.getInitParameter("staticURLPrefixes")).thenReturn("/static");
		when(request.getServletPath()).thenReturn("/static/app.js");
		filter.init(config);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
	}

	@Test
	void doFilterProcessesDynamicRequestWithNoSessionUser() throws Exception {
		RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/smartedit");
		when(request.getSession(false)).thenReturn(null);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		assertNull(WebContainer.getHttpServletRequestResponse());
	}

	@Test
	void doFilterClearsWebContainerWhenChainThrows() throws Exception {
		RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		ServletException failure = new ServletException("downstream");

		when(request.getServletPath()).thenReturn("/smartedit");
		when(request.getSession(false)).thenReturn(null);
		org.mockito.Mockito.doThrow(failure).when(chain).doFilter(request, response);

		ServletException thrown = assertThrows(ServletException.class, () -> filter.doFilter(request, response, chain));

		assertSame(failure, thrown);
		assertNull(WebContainer.getHttpServletRequestResponse());
	}

}
