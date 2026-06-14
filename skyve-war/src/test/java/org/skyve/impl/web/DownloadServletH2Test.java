package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class DownloadServletH2Test {
	@Test
	void doGetWritesGenericHtmlErrorWhenConversationIsMissing() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		doReturn(output.stream()).when(response).getOutputStream();

		new DownloadServlet().doGet(request, response);

		String body = output.text();
		assertTrue(body.contains("An error occured whilst processing your report."), body);
		verify(response).setContentType(MimeType.html.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
	}

	@Test
	void doHeadWritesHeadersWithoutGenericHtmlBodyWhenConversationIsMissing() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);

		new TestableDownloadServlet().doHead(request, response);

		verify(response).setContentType(MimeType.html.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
		verify(response, never()).getOutputStream();
	}

	private static final class TestableDownloadServlet extends DownloadServlet {
		private static final long serialVersionUID = 1L;

		@Override
		public void doHead(HttpServletRequest request, HttpServletResponse response)
		throws jakarta.servlet.ServletException, IOException {
			super.doHead(request, response);
		}
	}

	private static final class CapturedOutput {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

		private ServletOutputStream stream() {
			return new ServletOutputStream() {
				@Override
				public void write(int b) throws IOException {
					bytes.write(b);
				}

				@Override
				public boolean isReady() {
					return true;
				}

				@Override
				public void setWriteListener(WriteListener writeListener) {
					// Not needed for these synchronous tests.
				}
			};
		}

		private String text() {
			return bytes.toString(java.nio.charset.StandardCharsets.UTF_8);
		}
	}
}
