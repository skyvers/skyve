package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;

@SuppressWarnings("static-method")
class UTF8CharacterEncodingFilterTest {

	@Test
	void doFilterSetsRequestEncodingWhenNull() throws Exception {
		UTF8CharacterEncodingFilter filter = new UTF8CharacterEncodingFilter();
		ServletRequest request = mock(ServletRequest.class);
		when(request.getCharacterEncoding()).thenReturn(null);
		ServletResponse response = mock(ServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(request).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(chain).doFilter(request, response);
	}

	@Test
	void doFilterSkipsRequestEncodingWhenAlreadySet() throws Exception {
		UTF8CharacterEncodingFilter filter = new UTF8CharacterEncodingFilter();
		ServletRequest request = mock(ServletRequest.class);
		when(request.getCharacterEncoding()).thenReturn("ISO-8859-1");
		ServletResponse response = mock(ServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		// Should not override existing encoding on request
		verify(request).getCharacterEncoding();
		// But response encoding is always set
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(chain).doFilter(request, response);
	}

	@Test
	void initAndDestroyAreNoOps() {
		UTF8CharacterEncodingFilter filter = new UTF8CharacterEncodingFilter();
		assertDoesNotThrow(() -> filter.init(mock(FilterConfig.class)));
		assertDoesNotThrow(filter::destroy);
	}

	// ===== ExcludeStaticFilter =====

	private static final class TestExcludeFilter extends ExcludeStaticFilter {
		@Override
		public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) {
			// no-op
		}
	}

	@Test
	void initParsesStaticUrlPrefixes() throws Exception {
		TestExcludeFilter filter = new TestExcludeFilter();
		FilterConfig config = mock(FilterConfig.class);
		ServletContext context = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(context);
		when(context.getInitParameter("staticURLPrefixes")).thenReturn("/javax.faces.resource/\n/css/");

		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getServletPath()).thenReturn("/css/styles.css");
		assertTrue(filter.staticURLPrefix(request));
	}

	@Test
	void staticUrlPrefixMatchesServletPath() throws Exception {
		TestExcludeFilter filter = new TestExcludeFilter();
		FilterConfig config = mock(FilterConfig.class);
		ServletContext context = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(context);
		when(context.getInitParameter("staticURLPrefixes")).thenReturn("/javax.faces.resource/");

		filter.init(config);

		HttpServletRequest facesRequest = mock(HttpServletRequest.class);
		when(facesRequest.getServletPath()).thenReturn("/javax.faces.resource/primefaces.js");
		assertTrue(filter.staticURLPrefix(facesRequest));

		HttpServletRequest appRequest = mock(HttpServletRequest.class);
		when(appRequest.getServletPath()).thenReturn("/app/page.xhtml");
		assertFalse(filter.staticURLPrefix(appRequest));
	}

	@Test
	void staticUrlPrefixReturnsFalseWhenNoConfig() throws Exception {
		TestExcludeFilter filter = new TestExcludeFilter();
		FilterConfig config = mock(FilterConfig.class);
		ServletContext context = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(context);
		when(context.getInitParameter("staticURLPrefixes")).thenReturn(null);

		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getServletPath()).thenReturn("/anything.js");
		assertFalse(filter.staticURLPrefix(request));
	}
}
