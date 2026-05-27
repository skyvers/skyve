package org.skyve.impl.web.filter.rest;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.junit.jupiter.api.Test;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;

class BasicAuthFilterTest {

	private static class CapturingOutputStream extends ServletOutputStream {
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
		public void setWriteListener(WriteListener l) {
			// no-op
		}

		String asString() {
			return buf.toString(StandardCharsets.UTF_8);
		}
	}

	@Test
	void noAuthorizationHeaderReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn(null);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(eq(HttpServletResponse.SC_UNAUTHORIZED));
	}

	@Test
	void nonBasicAuthorizationHeaderReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		when(request.getHeader("Authorization")).thenReturn("Bearer sometoken");
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(eq(HttpServletResponse.SC_UNAUTHORIZED));
	}

	@Test
	void emptyUsernameInCredentialsReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		// Credentials ":password" => username is empty string => null after processStringValue
		String credentials = Base64.getMimeEncoder().encodeToString(":password".getBytes(StandardCharsets.UTF_8));
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(eq(HttpServletResponse.SC_UNAUTHORIZED));
	}

	@Test
	void emptyPasswordInCredentialsReturnsUnauthorized() throws IOException, jakarta.servlet.ServletException {
		BasicAuthFilter filter = new BasicAuthFilter();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		FilterChain chain = mock(FilterChain.class);

		when(request.getServletPath()).thenReturn("/api/json/admin/User/1");
		// Credentials "username:" => password is empty string => null after processStringValue
		String credentials = Base64.getMimeEncoder().encodeToString("username:".getBytes(StandardCharsets.UTF_8));
		when(request.getHeader("Authorization")).thenReturn("Basic " + credentials);
		CapturingOutputStream out = new CapturingOutputStream();
		when(response.getOutputStream()).thenReturn(out);
		when(response.getContentType()).thenReturn(MediaType.APPLICATION_JSON);

		filter.doFilter(request, response, chain);

		verify(response).setStatus(eq(HttpServletResponse.SC_UNAUTHORIZED));
	}
}
