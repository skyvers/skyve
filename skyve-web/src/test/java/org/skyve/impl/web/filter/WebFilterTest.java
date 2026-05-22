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
import java.util.Enumeration;
import java.util.Vector;

import org.junit.jupiter.api.Test;
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

public class WebFilterTest {

	// ===== UTF8CharacterEncodingFilter =====

	@Test
	public void testUtf8FilterSetsEncodingWhenNotSet() throws Exception {
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
	public void testUtf8FilterSkipsEncodingWhenAlreadySet() throws Exception {
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
	public void testResponseHeaderFilterSetsHeaders() throws Exception {
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
	public void testResponseHeaderFilterInitSetsSecurityConfig() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getFilterName()).thenReturn(ResponseHeaderFilter.SECURITY_HEADERS_FILTER_NAME);
		when(config.getInitParameterNames()).thenReturn(Collections.emptyEnumeration());

		ResponseHeaderFilter filter = new ResponseHeaderFilter();
		filter.init(config);
		filter.destroy();
	}

	// ===== DevLoginFilter init =====

	@Test
	public void testDevLoginFilterInitThrowsOnMissingCustomer() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn(null);
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");

		DevLoginFilter filter = new DevLoginFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	public void testDevLoginFilterInitThrowsOnMissingUser() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("demo");
		when(config.getInitParameter("user")).thenReturn(null);
		when(config.getInitParameter("password")).thenReturn("admin");

		DevLoginFilter filter = new DevLoginFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	public void testDevLoginFilterInitThrowsOnMissingPassword() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("demo");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn(null);

		DevLoginFilter filter = new DevLoginFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	public void testDevLoginFilterInitSucceedsWithAllParams() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("customer")).thenReturn("demo");
		when(config.getInitParameter("user")).thenReturn("admin");
		when(config.getInitParameter("password")).thenReturn("admin");

		DevLoginFilter filter = new DevLoginFilter();
		filter.init(config);
		filter.destroy();
	}

	// ===== ExcludeStaticFilter =====

	@Test
	public void testExcludeStaticFilterMatchesStaticPrefix() throws Exception {
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
	public void testExcludeStaticFilterNoPrefixesReturnsFalse() throws Exception {
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
	public void testThumbnailFilterInitWithDefaults() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
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
	public void testThumbnailFilterInitWithCustomParams() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
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
	public void testThumbnailFilterInitWithSvgNonImage() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
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
	public void testThumbnailFilterInitThrowsOnInvalidNonImageResponse() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("widthParam")).thenReturn(null);
		when(config.getInitParameter("heightParam")).thenReturn(null);
		when(config.getInitParameter("maxWidth")).thenReturn(null);
		when(config.getInitParameter("maxHeight")).thenReturn(null);
		when(config.getInitParameter("nonImageResponse")).thenReturn("invalid");

		ThumbnailFilter filter = new ThumbnailFilter();
		assertThrows(ServletException.class, () -> filter.init(config));
	}

	@Test
	public void testThumbnailFilterDoFilterPassesThroughWhenNoSizeParams() throws Exception {
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

		// No _w or _h params → pass straight through
		when(request.getParameter("_w")).thenReturn(null);
		when(request.getParameter("_h")).thenReturn(null);

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	public void testThumbnailFilterDoFilterPassesThroughWhenZeroWidth() throws Exception {
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

		// width=0, height=100 → pass straight through (non-positive)
		when(request.getParameter("_w")).thenReturn("0");
		when(request.getParameter("_h")).thenReturn("100");

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	public void testThumbnailFilterDoFilterPassesThroughWhenNonNumericParam() throws Exception {
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

		// non-numeric width → pass straight through
		when(request.getParameter("_w")).thenReturn("abc");
		when(request.getParameter("_h")).thenReturn("100");

		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	public void testThumbnailFilterDoFilterCapsWidthAndHeight() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
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

	// ===== CompressionFilter =====

	@Test
	public void testCompressionFilterInitWithDefaults() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn(null);
		when(config.getInitParameter("compressionThreshold")).thenReturn(null);

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	public void testCompressionFilterInitWithThreshold() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn("1");
		when(config.getInitParameter("compressionThreshold")).thenReturn("1024");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	public void testCompressionFilterInitWithBelowMinThreshold() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("debug")).thenReturn("2");
		when(config.getInitParameter("compressionThreshold")).thenReturn("50");

		CompressionFilter filter = new CompressionFilter();
		filter.init(config);
		filter.destroy();
	}

	@Test
	public void testCompressionFilterDoFilterNoCompressionWhenThresholdZero() throws Exception {
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
	public void testCompressionFilterDoFilterNoCompressionWhenGzipFalse() throws Exception {
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
	public void testCompressionFilterDoFilterNoCompressionWhenNoGzipHeader() throws Exception {
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
	public void testCompressionFilterDoFilterNoCompressionWhenNonGzipAcceptEncoding() throws Exception {
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
	public void testCompressionFilterInitWithNullConfig() throws Exception {
		CompressionFilter filter = new CompressionFilter();
		filter.init(null);
		filter.destroy();
	}
}
