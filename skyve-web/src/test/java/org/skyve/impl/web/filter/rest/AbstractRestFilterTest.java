package org.skyve.impl.web.filter.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.persistence.Persistence;

import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;

import java.util.Base64;

@SuppressWarnings({"static-method", "resource"})
class AbstractRestFilterTest {
	@Test
	void jsonErrorsAreEscapedAndKeepStatusAndAuthenticationHeader() throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);
		when(response.getOutputStream()).thenReturn(output);

		AbstractRestFilter.error(null,
									response,
									HttpServletResponse.SC_UNAUTHORIZED,
									"Skyve REST",
									"Bad \"message\"\n<script>");

		verify(response).resetBuffer();
		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(response).setHeader("WWW-Authenticate", "Basic realm=\"Skyve REST\"");
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response).setContentType(MediaType.APPLICATION_JSON);
		verify(response).flushBuffer();
		assertEquals("{\"error\":\"Bad \\\"message\\\"\\n<script>\"}", output.asString());
	}

	@Test
	void xmlErrorsAreEscapedAndRollbackPersistence() throws IOException {
		Persistence persistence = mock(Persistence.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_XML);
		when(response.getOutputStream()).thenReturn(output);

		AbstractRestFilter.error(persistence,
									response,
									HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
									WebErrorUtil.genericMessage("ref-1") + " <detail>");

		verify(persistence).rollback();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(response).setContentType(MediaType.APPLICATION_XML);
		assertTrue(output.asString().contains("<error>An error occurred while processing your request."));
		assertTrue(output.asString().contains("Reference: ref-1 &lt;detail&gt;</error>"));
	}

	@Test
	void missingExistingContentTypeDefaultsToJson() throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(response.getContentType()).thenReturn(null);
		when(response.getOutputStream()).thenReturn(output);

		AbstractRestFilter.error(response, "Generic failure");

		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(response).setContentType(MediaType.APPLICATION_JSON);
		assertEquals("{\"error\":\"Generic failure\"}", output.asString());
	}

	@Test
	void unsetContentTypeUsesXmlForXmlRestRequest() throws IOException {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(request.getRequestURI()).thenReturn("/skyve/api/xml/admin/User/123");
		when(response.getContentType()).thenReturn(null, MediaType.APPLICATION_XML);
		when(response.getOutputStream()).thenReturn(output);

		AbstractRestFilter.error(null, request, response, HttpServletResponse.SC_UNAUTHORIZED, "Skyve", "No credentials");

		verify(response, atLeastOnce()).setContentType(MediaType.APPLICATION_XML);
		assertTrue(output.asString().contains("<error>No credentials</error>"));
	}

	private static final class CapturingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream delegate = new ByteArrayOutputStream();

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// Not required for these synchronous unit tests.
		}

		@Override
		public void write(int b) {
			delegate.write(b);
		}

		private String asString() {
			return delegate.toString(StandardCharsets.UTF_8);
		}
	}

	// ===== ForbiddenFilter =====

	@Test
	void forbiddenFilterSendsForbiddenWhenNotUnsecured() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(request.getServletPath()).thenReturn("/api/secure");
		when(response.getOutputStream()).thenReturn(output);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		ForbiddenFilter filter = new ForbiddenFilter();
		FilterConfig config = mock(FilterConfig.class);
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void forbiddenFilterPassesThroughWhenPathIsUnsecured() throws Exception {
		FilterConfig config = mock(FilterConfig.class);
		when(config.getInitParameter("unsecured")).thenReturn("/api/public");
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		when(request.getServletPath()).thenReturn("/api/public/resource");

		ForbiddenFilter filter = new ForbiddenFilter();
		filter.init(config);
		filter.doFilter(request, response, chain);

		verify(chain).doFilter(request, response);
	}

	@Test
	void abstractRestFilterInitWithEmptyConfigDoesNotThrow() throws Exception {
		ForbiddenFilter filter = Assertions.assertDoesNotThrow(ForbiddenFilter::new);
		filter.init(mock(FilterConfig.class));
		filter.destroy();
	}

	// ===== BasicAuthFilter early-return paths (no H2 needed) =====

	@Test
	void basicAuthFilterNullAuthorizationHeaderSends401() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getHeader("Authorization")).thenReturn(null);
		when(response.getOutputStream()).thenReturn(output);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void basicAuthFilterNonBasicAuthorizationSends401() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getHeader("Authorization")).thenReturn("Bearer sometoken");
		when(response.getOutputStream()).thenReturn(output);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void basicAuthFilterEmptyUsernameSends401() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		// encode ":password" — empty username
		String encoded = Base64.getEncoder().encodeToString(":password".getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getHeader("Authorization")).thenReturn("Basic " + encoded);
		when(response.getOutputStream()).thenReturn(output);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(chain, never()).doFilter(request, response);
	}

	@Test
	void basicAuthFilterEmptyPasswordSends401() throws Exception {
		BasicAuthFilter filter = new BasicAuthFilter();
		filter.init(mock(FilterConfig.class));

		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		// encode "user:" — empty password
		String encoded = Base64.getEncoder().encodeToString("user:".getBytes(StandardCharsets.UTF_8));
		when(request.getServletPath()).thenReturn("/api/secure");
		when(request.getHeader("Authorization")).thenReturn("Basic " + encoded);
		when(response.getOutputStream()).thenReturn(output);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(HttpServletResponse.SC_UNAUTHORIZED);
		verify(chain, never()).doFilter(request, response);
	}
}
