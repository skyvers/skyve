package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class ExportAndDownloadServletFallbackTest {
	@Test
	void downloadWithoutConversationReturnsHtmlFallback() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		try (CapturingServletOutputStream output = new CapturingServletOutputStream()) {
			HttpServletResponse response = mock(HttpServletResponse.class);
			doReturn(output).when(response).getOutputStream();

			new DownloadServlet().doGet(request, response);

			verify(response).setContentType(MimeType.html.toString());
			verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
			assertFallbackBody(output);
		}
	}

	@Test
	void bizportExportWithoutConversationReturnsHtmlFallback() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		try (CapturingServletOutputStream output = new CapturingServletOutputStream()) {
			HttpServletResponse response = mock(HttpServletResponse.class);
			doReturn(output).when(response).getOutputStream();

			new BizportExportServlet().doGet(request, response);

			verify(response).setContentType(MimeType.html.toString());
			verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
			assertFallbackBody(output);
		}
	}

	private static void assertFallbackBody(CapturingServletOutputStream output) {
		String body = output.asString();
		assertTrue(body.startsWith("<html><head/><body><h3>"));
		assertTrue(body.contains("An error occured whilst processing your report."));
		assertTrue(body.endsWith("</body></html>"));
	}

	private static final class CapturingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

		@Override
		public void write(int b) {
			bytes.write(b);
		}

		@Override
		public void write(byte[] b, int off, int len) {
			bytes.write(b, off, len);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// Synchronous test stream.
		}

		String asString() {
			return bytes.toString(StandardCharsets.UTF_8);
		}
	}
}
