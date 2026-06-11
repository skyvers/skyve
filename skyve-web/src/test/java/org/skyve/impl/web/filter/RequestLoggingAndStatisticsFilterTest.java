package org.skyve.impl.web.filter;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.cache.CSRFTokenCacheConfig;
import org.skyve.cache.ConversationCacheConfig;
import org.skyve.cache.GeoIPCacheConfig;
import org.skyve.cache.SessionCacheConfig;
import org.skyve.impl.cache.DefaultCaching;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebContainer;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletContext;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "boxing"})
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

	@Test
	void doFilterLogsTraceRequestDetailsWithoutChangingRequestFlow(@TempDir Path cacheDirectory) throws Exception {
		boolean originalHttpTrace = UtilImpl.HTTP_TRACE;
		boolean originalCommandTrace = UtilImpl.COMMAND_TRACE;
		boolean originalForceNonPersistentCaching = UtilImpl.FORCE_NON_PERSISTENT_CACHING;
		String originalCacheDirectory = UtilImpl.CACHE_DIRECTORY;
		ConversationCacheConfig originalConversationCache = UtilImpl.CONVERSATION_CACHE;
		CSRFTokenCacheConfig originalCsrfTokenCache = UtilImpl.CSRF_TOKEN_CACHE;
		GeoIPCacheConfig originalGeoIpCache = UtilImpl.GEO_IP_CACHE;
		SessionCacheConfig originalSessionCache = UtilImpl.SESSION_CACHE;
		try {
			UtilImpl.HTTP_TRACE = true;
			UtilImpl.COMMAND_TRACE = true;
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = true;
			UtilImpl.CACHE_DIRECTORY = cacheDirectory.toString() + File.separator;
			UtilImpl.CONVERSATION_CACHE = new ConversationCacheConfig(10, 1);
			UtilImpl.CSRF_TOKEN_CACHE = new CSRFTokenCacheConfig(10, 1);
			UtilImpl.GEO_IP_CACHE = new GeoIPCacheConfig(10, 1);
			UtilImpl.SESSION_CACHE = new SessionCacheConfig(10, 1);
			DefaultCaching.get().shutdown();
			DefaultCaching.get().startup();

			RequestLoggingAndStatisticsFilter filter = new RequestLoggingAndStatisticsFilter();
			HttpServletRequest request = mock(HttpServletRequest.class);
			HttpServletResponse response = mock(HttpServletResponse.class);
			FilterChain chain = mock(FilterChain.class);

			when(request.getServletPath()).thenReturn("/smartedit");
			when(request.getSession(false)).thenReturn(null);
			when(request.getContextPath()).thenReturn("/skyve");
			when(request.getLocalAddr()).thenReturn("127.0.0.1");
			when(request.getLocalName()).thenReturn("localhost");
			when(request.getLocalPort()).thenReturn(8080);
			when(request.getMethod()).thenReturn("POST");
			when(request.getPathInfo()).thenReturn("/edit");
			when(request.getPathTranslated()).thenReturn("/tmp/edit");
			when(request.getProtocol()).thenReturn("HTTP/1.1");
			when(request.getQueryString()).thenReturn("q=1");
			when(request.getRemoteAddr()).thenReturn("127.0.0.2");
			when(request.getRemoteHost()).thenReturn("client");
			when(request.getRemotePort()).thenReturn(42000);
			when(request.getRemoteUser()).thenReturn("jane");
			when(request.getRequestedSessionId()).thenReturn("session-id");
			when(request.getRequestURI()).thenReturn("/skyve/smartedit");
			when(request.getRequestURL()).thenReturn(new StringBuffer("http://localhost:8080/skyve/smartedit"));
			when(request.getScheme()).thenReturn("http");
			when(request.getServerName()).thenReturn("localhost");
			when(request.getServerPort()).thenReturn(8080);
			when(request.getUserPrincipal()).thenReturn(() -> "jane");

			when(request.getParameterNames()).thenReturn(Collections.enumeration(Arrays.asList("password", "comment", "blob", "note")));
			when(request.getParameterValues("comment")).thenReturn(new String[] {"ordinary"});
			when(request.getParameterValues("blob")).thenReturn(new String[] {"x".repeat(51201)});
			when(request.getParameterValues("note")).thenReturn(new String[] {"contains password text"});
			when(request.getHeaderNames()).thenReturn(Collections.enumeration(Arrays.asList("User-Agent", "X-Test")));
			when(request.getHeader("User-Agent")).thenReturn("JUnit");
			when(request.getHeader("X-Test")).thenReturn("value");

			filter.doFilter(request, response, chain);

			verify(chain).doFilter(request, response);
			assertNull(WebContainer.getHttpServletRequestResponse());
		}
		finally {
			DefaultCaching.get().shutdown();
			UtilImpl.HTTP_TRACE = originalHttpTrace;
			UtilImpl.COMMAND_TRACE = originalCommandTrace;
			UtilImpl.FORCE_NON_PERSISTENT_CACHING = originalForceNonPersistentCaching;
			UtilImpl.CACHE_DIRECTORY = originalCacheDirectory;
			UtilImpl.CONVERSATION_CACHE = originalConversationCache;
			UtilImpl.CSRF_TOKEN_CACHE = originalCsrfTokenCache;
			UtilImpl.GEO_IP_CACHE = originalGeoIpCache;
			UtilImpl.SESSION_CACHE = originalSessionCache;
			WebContainer.clear();
		}
	}

}
