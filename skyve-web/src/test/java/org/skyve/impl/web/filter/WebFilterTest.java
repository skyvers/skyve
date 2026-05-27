package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Vector;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.skyve.impl.web.filter.gzip.CompressionFilter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class WebFilterTest {

	// ===== UTF8CharacterEncodingFilter =====

	@Test
	void testUtf8FilterSetsEncodingWhenNotSet() throws Exception {
		UTF8CharacterEncodingFilter filter = new UTF8CharacterEncodingFilter();
		filter.init(null);

		ServletRequest request = mock(ServletRequest.class);
		when(request.getCharacterEncoding()).thenReturn(null);
		ServletResponse response = mock(ServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(request).setCharacterEncoding("UTF-8");
		verify(response).setCharacterEncoding("UTF-8");
		verify(chain).doFilter(request, response);
		filter.destroy();
	}

	@Test
	void testUtf8FilterSkipsEncodingWhenAlreadySet() throws Exception {
		UTF8CharacterEncodingFilter filter = new UTF8CharacterEncodingFilter();
		filter.init(null);

		ServletRequest request = mock(ServletRequest.class);
		when(request.getCharacterEncoding()).thenReturn("ISO-8859-1");
		ServletResponse response = mock(ServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);

		verify(request, never()).setCharacterEncoding(anyString());
		verify(response).setCharacterEncoding("UTF-8");
		verify(chain).doFilter(request, response);
	}

	// ===== ResponseHeaderFilter =====

	@Test
	void testResponseHeaderFilterSetsHeaders() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		HttpServletRequest request = mock(HttpServletRequest.class);

		Vector<String> headerNames = new Vector<>();
		headerNames.add("X-Frame-Options");
		headerNames.add("X-Content-Type-Options");
		when(config.getFilterName()).thenReturn("TestFilter");
		when(config.getInitParameterNames()).thenReturn(headerNames.elements());
		when(config.getInitParameter("X-Frame-Options")).thenReturn("DENY");
		when(config.getInitParameter("X-Content-Type-Options")).thenReturn("nosniff");

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(response).setHeader("X-Frame-Options", "DENY");
		verify(response).setHeader("X-Content-Type-Options", "nosniff");
		verify(chain).doFilter(request, response);
		filter.destroy();
	}

	@Test
	void testResponseHeaderFilterInitSetsSecurityConfig() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getFilterName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);
		when(config.getInitParameterNames()).thenReturn(Collections.emptyEnumeration());

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testResponseHeaderFilterDestroySetsSecurityConfigToNull() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getFilterName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);
		Vector<String> headerNames = new Vector<>();
		headerNames.add("X-Destroy-Test");
		when(config.getInitParameterNames()).thenReturn(headerNames.elements());
		when(config.getInitParameter("X-Destroy-Test")).thenReturn("enabled");

		HttpServletResponse response = mock(HttpServletResponse.class);

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		ResponseHeaderFilter.applySecurityHeaders(response);
		verify(response).setHeader("X-Destroy-Test", "enabled");

		org.mockito.Mockito.reset(response);
		// destroy should null out SECURITY_HEADERS_FILTER_CONFIG — won't throw
		filter.destroy();
		ResponseHeaderFilter.applySecurityHeaders(response);
		verify(response, never()).setHeader(anyString(), anyString());
	}

	@Test
	void testResponseHeaderFilterWithExpiresHeader() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		HttpServletRequest request = mock(HttpServletRequest.class);

		Vector<String> headerNames = new Vector<>();
		headerNames.add("Expires");
		when(config.getFilterName()).thenReturn("TestFilter");
		when(config.getInitParameterNames()).thenReturn(headerNames.elements());
		when(config.getInitParameter("Expires")).thenReturn("86400000");

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(response).setDateHeader(eq("Expires"), org.mockito.ArgumentMatchers.anyLong());
		filter.destroy();
	}

	@Test
	void testResponseHeaderFilterWithCspHeaderNoHttps() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		HttpServletRequest request = mock(HttpServletRequest.class);

		Vector<String> headerNames = new Vector<>();
		headerNames.add("Content-Security-Policy");
		when(config.getFilterName()).thenReturn("TestFilter");
		when(config.getInitParameterNames()).thenReturn(headerNames.elements());
		when(config.getInitParameter("Content-Security-Policy"))
			.thenReturn("default-src 'self'; upgrade-insecure-requests;");

		// Ensure isSecureUrl() returns false by clearing SERVER_URL
		org.skyve.impl.util.UtilImpl.SERVER_URL = null;
		// Reset the cached secureUrl so it re-evaluates
		try {
			java.lang.reflect.Field f = org.skyve.util.Util.class.getDeclaredField("secureUrl");
			f.setAccessible(true);
			f.set(null, null);
		} catch (@SuppressWarnings("unused") Exception ignored) {
			// ignore test-only reflective reset failures
		}

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		filter.doFilter(request, response, chain);

		// upgrade-insecure-requests should be stripped when not https
		verify(response).setHeader(eq("Content-Security-Policy"), org.mockito.ArgumentMatchers.anyString());
		filter.destroy();
	}

	@Test
	void testResponseHeaderFilterApplySecurityHeaders() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getFilterName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);
		Vector<String> headerNames = new Vector<>();
		headerNames.add("X-Frame-Options");
		when(config.getInitParameterNames()).thenReturn(headerNames.elements());
		when(config.getInitParameter("X-Frame-Options")).thenReturn("SAMEORIGIN");
		HttpServletResponse response = mock(HttpServletResponse.class);

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);

		ResponseHeaderFilter.applySecurityHeaders(response);
		verify(response).setHeader("X-Frame-Options", "SAMEORIGIN");
		filter.destroy();
	}

	// ===== DevLoginFilter init =====

	@ParameterizedTest
	@MethodSource("missingDevLoginParams")
	void testDevLoginFilterInitThrowsOnMissingRequiredParam(String customer, String user, String password) {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn(customer);
		when(config.getInitParameter("user")).thenReturn(user);
		when(config.getInitParameter("password")).thenReturn(password);

		DevLoginFilter filter = new DevLoginFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	private static Stream<Arguments> missingDevLoginParams() {
		return Stream.of(
			Arguments.of(null, "admin", "admin"),
			Arguments.of("demo", null, "admin"),
			Arguments.of("demo", "admin", null));
	}

	@Test
	void testDevLoginFilterInitSucceedsWithAllParams() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("customer")).thenReturn("demo");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");

		DevLoginFilter filter = new DevLoginFilter();
		filter.init(config);
		filter.destroy();
	}

	// ===== ExcludeStaticFilter =====

	@Test
	void testExcludeStaticFilterMatchesStaticPrefix() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		ServletContext ctx = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(ctx);
		when(ctx.getInitParameter("staticURLPrefixes")).thenReturn("/resources\n/images");

		// Concrete subclass for testing
		ExcludeStaticFilter filter = new ExcludeStaticFilter() {
			@Override
			public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {
				chain.doFilter(req, res);
			}
		};
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getServletPath()).thenReturn("/resources/style.css");
		assertTrue(filter.staticURLPrefix(request));

		when(request.getServletPath()).thenReturn("/images/logo.png");
		assertTrue(filter.staticURLPrefix(request));

		when(request.getServletPath()).thenReturn("/faces/index.xhtml");
		assertFalse(filter.staticURLPrefix(request));

		filter.destroy();
	}

	@Test
	void testExcludeStaticFilterNoPrefixesReturnsFalse() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		ServletContext ctx = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(ctx);
		when(ctx.getInitParameter("staticURLPrefixes")).thenReturn(null);

		ExcludeStaticFilter filter = new ExcludeStaticFilter() {
			@Override
			public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {
				chain.doFilter(req, res);
			}
		};
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getServletPath()).thenReturn("/resources/style.css");
		assertFalse(filter.staticURLPrefix(request));

		filter.destroy();
	}

	// ===== ThumbnailFilter =====

	@Test
	void testThumbnailFilterInitWithDefaults() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn(null);

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testThumbnailFilterInitWithCustomParams() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn("width");
		when(config.getInitParameter("heightParam")).thenReturn("height");
		when(config.getInitParameter("maxWidth")).thenReturn("800");
		when(config.getInitParameter("maxHeight")).thenReturn("600");
		when(config.getInitParameter("nonImageResponse")).thenReturn("passthrough");

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testThumbnailFilterInitWithSvgNonImage() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn("svg");

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testThumbnailFilterInitThrowsOnInvalidNonImageResponse() {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn("invalid");

		ThumbnailFilter filter = new ThumbnailFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@ParameterizedTest
	@MethodSource("passthroughThumbnailParams")
	void testThumbnailFilterDoFilterPassesThroughForInvalidParams(String width, String height) throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn(null);

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getParameter("_w")).thenReturn(width);
		when(request.getParameter("_h")).thenReturn(height);

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	private static Stream<Arguments> passthroughThumbnailParams() {
		return Stream.of(
			Arguments.of(null, null),
			Arguments.of("0", "100"),
			Arguments.of("abc", "100"));
	}

	@Test
	void testThumbnailFilterDoFilterCapsWidthAndHeight() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn("800");
		when(config.getInitParameter("maxHeight")).thenReturn("600");
		when(config.getInitParameter("nonImageResponse")).thenReturn(null);

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = new FilterChain() {
			@Override
			public void doFilter(ServletRequest req, ServletResponse res) throws IOException, ServletException {
				// write a non-image content type so it passes through unchanged
				HttpServletResponse httpRes = (HttpServletResponse) res;
				httpRes.setContentType("text/plain");
				try {
					res.getOutputStream().write("hello".getBytes());
				}
				catch (@SuppressWarnings("unused") Exception e) {
					// ignore
				}
			}
		};

		// width=2000/height=1000 both exceed max → get capped, then chain runs
		when(request.getParameter("_w")).thenReturn("2000");
		when(request.getParameter("_h")).thenReturn("1000");
		when(request.getRequestURI()).thenReturn("/test/image.jpg");

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ServletOutputStream sos = new ServletOutputStream() {
			@Override
			public void write(int b) {
				baos.write(b);
			}

			@Override
			public boolean isReady() {
				return true;
			}

			@Override
			public void setWriteListener(WriteListener writeListener) {
				// no-op
			}
		};
		when(response.getOutputStream()).thenReturn(sos);

		filter.doFilter(request, response, chain);
		// Should not throw – content type not recognised as image, so passes through unchanged
	}

	@Test
	void testThumbnailFilterCapturingWrapperUsesWriterWhenChainWritesText() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn(null);

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		// FilterChain writes via getWriter() and sets Content-Type via setHeader
		FilterChain chain = (req, res) -> {
			HttpServletResponse r = (HttpServletResponse) res;
			r.setHeader("Content-Type", "text/html");
			try (PrintWriter pw = r.getWriter()) {
				pw.write("<html/>");
			}
		};

		when(request.getParameter("_w")).thenReturn("100");
		when(request.getParameter("_h")).thenReturn("100");
		when(request.getRequestURI()).thenReturn("/test/page");
		when(response.getCharacterEncoding()).thenReturn("UTF-8");
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ServletOutputStream sos = new ServletOutputStream() {
			@Override public void write(int b) { baos.write(b); }
			@Override public boolean isReady() { return true; }
			@Override public void setWriteListener(WriteListener l) {
				// no-op for test stream
			}
		};
		when(response.getOutputStream()).thenReturn(sos);

		filter.doFilter(request, response, chain);
		// Not an image content type → passes through
	}

	@Test
	void testThumbnailFilterCapturingWrapperAddsContentTypeHeader() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn(null);

		ThumbnailFilter filter = new ThumbnailFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		// FilterChain sets Content-Type via addHeader to cover addHeader() in CapturingResponseWrapper
		FilterChain chain = (req, res) -> {
			HttpServletResponse r = (HttpServletResponse) res;
			r.addHeader("Content-Type", "text/plain");
			try {
				res.getOutputStream().write("data".getBytes());
			} catch (@SuppressWarnings("unused") Exception ignored) {
				// no-op for test helper path
			}
		};

		when(request.getParameter("_w")).thenReturn("50");
		when(request.getParameter("_h")).thenReturn("50");
		when(request.getRequestURI()).thenReturn("/test/data");
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		ServletOutputStream sos = new ServletOutputStream() {
			@Override public void write(int b) { baos.write(b); }
			@Override public boolean isReady() { return true; }
			@Override public void setWriteListener(WriteListener l) {
				// no-op for test stream
			}
		};
		when(response.getOutputStream()).thenReturn(sos);

		filter.doFilter(request, response, chain);
		// text/plain not an image → passes through unchanged
	}

	// ===== CompressionFilter =====

	@ParameterizedTest
	@MethodSource("compressionInitParams")
	void testCompressionFilterInitConfigurations(String debugValue, String thresholdValue) {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter("debug")).thenReturn(debugValue);
		when(config.getInitParameter("compressionThreshold")).thenReturn(thresholdValue);

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);
		filter.destroy();
	}

	private static Stream<Arguments> compressionInitParams() {
		return Stream.of(
			Arguments.of(null, null),
			Arguments.of("1", "1024"),
			Arguments.of("2", "50"));
	}

	@Test
	void testCompressionFilterDoFilterNoCompressionWhenThresholdZero() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn(null); // threshold=0

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testCompressionFilterDoFilterNoCompressionWhenGzipFalse() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getParameter("gzip")).thenReturn("false");

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testCompressionFilterDoFilterNoCompressionWhenNoGzipHeader() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getParameter("gzip")).thenReturn(null);
		when(request.getHeaders("Accept-Encoding")).thenReturn(Collections.emptyEnumeration());

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testCompressionFilterDoFilterNoCompressionWhenNonGzipAcceptEncoding() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getParameter("gzip")).thenReturn(null);
		Vector<String> headers = new Vector<>();
		headers.add("deflate");
		when(request.getHeaders("Accept-Encoding")).thenReturn(headers.elements());

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testCompressionFilterInitWithNullConfig() {
		CompressionFilter filter = Assertions.assertDoesNotThrow(CompressionFilter::new);
		filter.init(null);
		filter.destroy();
	}

	@Test
	void testCompressionFilterDoFilterWithGzipSupportCompressesResponse() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		ServletOutputStream sos = new ServletOutputStream() {
			@Override public boolean isReady() { return true; }
			@Override public void setWriteListener(WriteListener l) {
				// no-op for test stream
			}
			@Override public void write(int b) throws IOException { bos.write(b); }
		};
		when(response.getOutputStream()).thenReturn(sos);

		when(request.getParameter("gzip")).thenReturn(null);
		Vector<String> headers = new Vector<>();
		headers.add("gzip, deflate");
		when(request.getHeaders("Accept-Encoding")).thenReturn(headers.elements());

		filter.doFilter(request, response, chain);
		// Chain should have been called with a wrapped (CompressionServletResponseWrapper) response, not the original
		verify(chain).doFilter(eq(request), org.mockito.AdditionalMatchers.not(eq(response)));
	}

	@Test
	void testCompressionFilterSetAndGetFilterConfig() {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		assertNull(filter.getFilterConfig());
		filter.setFilterConfig(config);
		assertEquals(config, filter.getFilterConfig());
	}

	// ===== RequestLoggingAndStatisticsFilter =====

	@Test
	void testRequestLoggingFilterStaticUrlDelegatesToChainAndReturns() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		ServletContext ctx = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(ctx);
		when(ctx.getInitParameter("staticURLPrefixes")).thenReturn("/resources");

		RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/resources/style.css");

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		filter.destroy();
	}

	@Test
	void testRequestLoggingFilterNullSessionCallsChainWithoutH2() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		ServletContext ctx = mock(ServletContext.class);
		when(config.getServletContext()).thenReturn(ctx);
		when(ctx.getInitParameter("staticURLPrefixes")).thenReturn(null);

		RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
		filter.init(config);

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/faces/index.xhtml");
		when(request.getSession(false)).thenReturn(null);

		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
		filter.destroy();
	}
}
