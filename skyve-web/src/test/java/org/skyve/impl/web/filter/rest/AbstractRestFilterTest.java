package org.skyve.impl.web.filter.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.impl.web.WebErrorUtil;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.ws.rs.core.MediaType;

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
		verify(response).setCharacterEncoding(Util.UTF8);
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
}
