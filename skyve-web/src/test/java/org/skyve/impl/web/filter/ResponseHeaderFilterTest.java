package org.skyve.impl.web.filter;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Vector;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Unit tests for {@link ResponseHeaderFilter}.
 */
@SuppressWarnings("all")
class ResponseHeaderFilterTest {

	@Test
	void doFilterAppliesHeadersAndChains() throws Exception {
		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		FilterConfig config = mock(FilterConfig.class);
		Vector<String> names = new Vector<>();
		names.add("X-Custom-Header");
		when(config.getInitParameterNames()).thenReturn(names.elements());
		when(config.getInitParameter("X-Custom-Header")).thenReturn("custom-value");
		when(config.getFilterName()).thenReturn("TestFilter");
		filter.init(config);

		ServletRequest request = mock(ServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(response).setHeader("X-Custom-Header", "custom-value");
		verify(chain).doFilter(request, response);
	}

	@Test
	void applyHeadersExpiresSetsDateHeader() throws Exception {
		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		FilterConfig config = mock(FilterConfig.class);
		Vector<String> names = new Vector<>();
		names.add("Expires");
		when(config.getInitParameterNames()).thenReturn(names.elements());
		when(config.getInitParameter("Expires")).thenReturn("60000");
		filter.init(config);

		ServletRequest request = mock(ServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		ArgumentCaptor<Long> dateCaptor = ArgumentCaptor.forClass(Long.class);
		verify(response).setDateHeader(eq("Expires"), dateCaptor.capture());
		// Should be roughly current time + 60000
		long delta = dateCaptor.getValue().longValue() - System.currentTimeMillis();
		assertTrue(delta < 61000 && delta >= 0, "Expected near 60000 ms offset, got " + delta);
	}

	private static void assertTrue(boolean condition, String message) {
		if (!condition) {
			throw new AssertionError(message);
		}
	}
}
