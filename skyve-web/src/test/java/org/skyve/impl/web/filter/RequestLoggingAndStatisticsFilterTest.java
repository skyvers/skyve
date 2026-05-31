package org.skyve.impl.web.filter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
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
	}

}
