package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.impl.persistence.AbstractPersistence;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import modules.test.AbstractSkyveTest;

@SuppressWarnings({"static-method", "resource"})
class DownloadServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doGetWritesGenericHtmlErrorWhenConversationIsMissing() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn(null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(response.getOutputStream()).thenReturn(output.stream());

		new DownloadServlet().doGet(request, response);

		String body = output.text();
		assertTrue(body.contains("An error occured whilst processing your report."), body);
		verify(response).setContentType(MimeType.html.toString());
		verify(response).setCharacterEncoding(java.nio.charset.StandardCharsets.UTF_8.name());
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
