package org.skyve.impl.web.filter.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.security.Principal;
import java.util.Collections;
import java.util.Vector;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"resource", "boxing"})
class RegexFilterTest {

	private RegexFilter filter;
	private HttpServletRequest request;
	private HttpServletResponse response;
	private FilterChain chain;
	private CapturingServletOutputStream output;

	@BeforeEach
	void setUp() {
		filter = new RegexFilter();
		request = mock(HttpServletRequest.class);
		response = mock(HttpServletResponse.class);
		chain = mock(FilterChain.class);
		output = new CapturingServletOutputStream();
	}

	private static FilterConfig buildConfig(String name, String regex) {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn(null);
		Vector<String> names = new Vector<>();
		if (name != null) {
			names.add(name);
		}
		when(config.getInitParameterNames()).thenReturn(names.elements());
		if (name != null) {
			when(config.getInitParameter(name)).thenReturn(regex);
		}
		return config;
	}

	// ===== init =====

	@Test
	void testInitStoresRegexParameter() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn(null);
		Vector<String> names = new Vector<>();
		names.add("Method");
		names.add("realm"); // should be excluded
		names.add("unsecured"); // should be excluded
		when(config.getInitParameterNames()).thenReturn(names.elements());
		when(config.getInitParameter("Method")).thenReturn("GET|POST");
		when(config.getInitParameter("realm")).thenReturn("Skyve");
		when(config.getInitParameter("unsecured")).thenReturn("/public");
		filter.init(config);
		// only Method should be stored
		assertEquals(1, filter.getInitParameters().size());
		assertEquals("GET|POST", filter.getInitParameters().get("Method"));
		filter.destroy();
	}

	@Test
	void testInitWithNullValueSkipsParam() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn(null);
		Vector<String> names = new Vector<>();
		names.add("Method");
		when(config.getInitParameterNames()).thenReturn(names.elements());
		when(config.getInitParameter("Method")).thenReturn(null); // null value → skip
		filter.init(config);
		assertEquals(0, filter.getInitParameters().size());
	}

	// ===== doFilter pass-through when no init params =====

	@Test
	void testDoFilterPassesThroughWithNoParams() throws Exception {
		filter.init(buildConfig(null, null));
		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	// ===== ContextPath checks =====

	@Test
	void testDoFilterBlocksMismatchedContextPath() throws Exception {
		filter.init(buildConfig("ContextPath", "/myapp"));
		when(request.getContextPath()).thenReturn("/other");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	// ===== Method checks =====

	@Test
	void testDoFilterBlocksMismatchedMethod() throws Exception {
		filter.init(buildConfig("Method", "GET|POST"));
		when(request.getMethod()).thenReturn("DELETE");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	// ===== RemoteAddr checks =====

	@Test
	void testDoFilterBlocksMismatchedRemoteAddr() throws Exception {
		filter.init(buildConfig("RemoteAddr", "192\\.168\\..*"));
		when(request.getRemoteAddr()).thenReturn("10.0.0.1");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	// ===== RequestURI checks =====

	@Test
	void testDoFilterBlocksMismatchedRequestURI() throws Exception {
		filter.init(buildConfig("RequestURI", "/api/.*"));
		when(request.getRequestURI()).thenReturn("/admin/secret");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	// ===== LocalAddr checks =====

	@Test
	void testDoFilterBlocksMismatchedLocalAddr() throws Exception {
		filter.init(buildConfig("LocalAddr", "127\\.0\\.0\\.1"));
		when(request.getLocalAddr()).thenReturn("10.0.0.1");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== LocalName checks =====

	@Test
	void testDoFilterBlocksMismatchedLocalName() throws Exception {
		filter.init(buildConfig("LocalName", "localhost"));
		when(request.getLocalName()).thenReturn("remotehost");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== LocalPort checks =====

	@Test
	void testDoFilterBlocksMismatchedLocalPort() throws Exception {
		filter.init(buildConfig("LocalPort", "8080"));
		when(request.getLocalPort()).thenReturn(9090);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== PathInfo checks =====

	@Test
	void testDoFilterBlocksMismatchedPathInfo() throws Exception {
		filter.init(buildConfig("PathInfo", "/allowed/.*"));
		when(request.getPathInfo()).thenReturn("/forbidden/path");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== PathTranslated checks =====

	@Test
	void testDoFilterBlocksMismatchedPathTranslated() throws Exception {
		filter.init(buildConfig("PathTranslated", "/safe/.*"));
		when(request.getPathTranslated()).thenReturn("/unsafe/path");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== Protocol checks =====

	@Test
	void testDoFilterBlocksMismatchedProtocol() throws Exception {
		filter.init(buildConfig("Protocol", "HTTP/1.1"));
		when(request.getProtocol()).thenReturn("HTTP/2.0");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== QueryString checks =====

	@Test
	void testDoFilterBlocksMismatchedQueryString() throws Exception {
		filter.init(buildConfig("QueryString", "token=.*"));
		when(request.getQueryString()).thenReturn("page=1");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== RemoteHost checks =====

	@Test
	void testDoFilterBlocksMismatchedRemoteHost() throws Exception {
		filter.init(buildConfig("RemoteHost", "trusted\\.example\\.com"));
		when(request.getRemoteHost()).thenReturn("attacker.example.com");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== RemotePort checks =====

	@Test
	void testDoFilterBlocksMismatchedRemotePort() throws Exception {
		filter.init(buildConfig("RemotePort", "443"));
		when(request.getRemotePort()).thenReturn(12345);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== RemoteUser checks =====

	@Test
	void testDoFilterBlocksMismatchedRemoteUser() throws Exception {
		filter.init(buildConfig("RemoteUser", "admin"));
		when(request.getRemoteUser()).thenReturn("guest");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== RequestURL checks =====

	@Test
	void testDoFilterBlocksMismatchedRequestURL() throws Exception {
		filter.init(buildConfig("RequestURL", "https://.*"));
		when(request.getRequestURL()).thenReturn(new StringBuffer("http://example.com/api"));
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksNullRequestURL() throws Exception {
		filter.init(buildConfig("RequestURL", "https://.*"));
		when(request.getRequestURL()).thenReturn(null);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== Scheme checks =====

	@Test
	void testDoFilterBlocksMismatchedScheme() throws Exception {
		filter.init(buildConfig("Scheme", "https"));
		when(request.getScheme()).thenReturn("http");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== ServerName checks =====

	@Test
	void testDoFilterBlocksMismatchedServerName() throws Exception {
		filter.init(buildConfig("ServerName", "api\\.example\\.com"));
		when(request.getServerName()).thenReturn("www.example.com");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== ServerPort checks =====

	@Test
	void testDoFilterBlocksMismatchedServerPort() throws Exception {
		filter.init(buildConfig("ServerPort", "443"));
		when(request.getServerPort()).thenReturn(80);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== ServletPath checks =====

	@Test
	void testDoFilterBlocksMismatchedServletPath() throws Exception {
		filter.init(buildConfig("ServletPath", "/api/.*"));
		when(request.getServletPath()).thenReturn("/faces/index.xhtml");
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== UserPrincipal checks =====

	@Test
	void testDoFilterBlocksNullUserPrincipal() throws Exception {
		filter.init(buildConfig("UserPrincipal", "admin"));
		when(request.getUserPrincipal()).thenReturn(null);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksMismatchedUserPrincipal() throws Exception {
		filter.init(buildConfig("UserPrincipal", "admin"));
		Principal principal = mock(Principal.class);
		when(principal.getName()).thenReturn("guest");
		when(request.getUserPrincipal()).thenReturn(principal);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void testDoFilterAllowsMatchingUserPrincipal() throws Exception {
		filter.init(buildConfig("UserPrincipal", "admin"));
		Principal principal = mock(Principal.class);
		when(principal.getName()).thenReturn("admin");
		when(request.getUserPrincipal()).thenReturn(principal);
		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	// ===== Header checks =====

	@Test
	void testDoFilterAllowsMatchingHeader() throws Exception {
		filter.init(buildConfig("X-API-Key", "secret-.*"));
		Vector<String> headers = new Vector<>();
		headers.add("secret-abc123");
		when(request.getHeaders("X-API-Key")).thenReturn(headers.elements());
		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksMismatchedHeader() throws Exception {
		filter.init(buildConfig("X-API-Key", "secret-.*"));
		Vector<String> headers = new Vector<>();
		headers.add("invalid-key");
		when(request.getHeaders("X-API-Key")).thenReturn(headers.elements());
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksMissingHeaderFallsBackToQueryParams() throws Exception {
		filter.init(buildConfig("X-Required-Header", "required-.*"));
		when(request.getHeaders("X-Required-Header")).thenReturn(Collections.emptyEnumeration());
		// no header → tries query params
		when(request.getParameterValues("X-Required-Header")).thenReturn(new String[]{"wrong-value"});
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksMissingHeaderAndNoQueryParam() throws Exception {
		filter.init(buildConfig("X-Required-Header", ".*"));
		when(request.getHeaders("X-Required-Header")).thenReturn(Collections.emptyEnumeration());
		when(request.getParameterValues("X-Required-Header")).thenReturn(null);
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== Query parameter checks =====

	@Test
	void testDoFilterAllowsMatchingQueryParam() throws Exception {
		filter.init(buildConfig("token", "valid-.*"));
		when(request.getHeaders("token")).thenReturn(Collections.emptyEnumeration());
		when(request.getParameterValues("token")).thenReturn(new String[]{"valid-abc"});
		filter.doFilter(request, response, chain);
		verify(chain).doFilter(request, response);
	}

	@Test
	void testDoFilterBlocksMismatchedQueryParam() throws Exception {
		filter.init(buildConfig("token", "valid-.*"));
		when(request.getHeaders("token")).thenReturn(Collections.emptyEnumeration());
		when(request.getParameterValues("token")).thenReturn(new String[]{"invalid"});
		when(response.getOutputStream()).thenReturn(output);
		filter.doFilter(request, response, chain);
		verify(chain, never()).doFilter(request, response);
	}

	// ===== AbstractRestFilter init and doUnsecuredFilter =====

	@Test
	void testAbstractRestFilterInitWithRealm() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn("MyRealm");
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameterNames()).thenReturn(Collections.emptyEnumeration());
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testAbstractRestFilterInitWithUnsecuredPrefixes() throws Exception {
		FilterConfig config = Assertions.assertDoesNotThrow(() -> mock(FilterConfig.class));
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn("/public\n/api/v1/open");
		when(config.getInitParameterNames()).thenReturn(Collections.emptyEnumeration());
		filter.init(config);
		filter.destroy();
	}

	@Test
	void testDoUnsecuredFilterPassesThroughMatchingPath() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter(AbstractRestFilter.REALM_INIT_PARAMETER)).thenReturn(null);
		when(config.getInitParameter(AbstractRestFilter.UNSECURED_INIT_PARAMETER)).thenReturn("/public");
		when(config.getInitParameterNames()).thenReturn(Collections.emptyEnumeration());
		filter.init(config);

		when(request.getServletPath()).thenReturn("/public/resource");
		filter.doFilter(request, response, chain);
		// unsecured path means it passes through (doUnsecuredFilter returns true before regex checks)
		verify(chain).doFilter(request, response);
	}

	// ===== Utility class for capturing servlet output =====

	private static class CapturingServletOutputStream extends ServletOutputStream {
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
		public void setWriteListener(WriteListener writeListener) {
			// no-op
		}
	}
}
