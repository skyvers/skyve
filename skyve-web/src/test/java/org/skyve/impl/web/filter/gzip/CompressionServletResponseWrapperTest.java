package org.skyve.impl.web.filter.gzip;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class CompressionServletResponseWrapperTest {
	@Test
	void setContentTypeDelegatesToOriginalResponse() {
		HttpServletResponse response = mock(HttpServletResponse.class);
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(response);

		wrapper.setContentType("application/json");

		verify(response).setContentType("application/json");
	}

	@Test
	void getOutputStreamReturnsSameStreamAndBlocksWriter() throws Exception {
		CompressionServletResponseWrapper wrapper = wrapperWritingTo(new CapturedServletOutputStream());

		ServletOutputStream stream = wrapper.getOutputStream();

		assertSame(stream, wrapper.getOutputStream());
		assertThrows(IllegalStateException.class, wrapper::getWriter);
	}

	@Test
	void getWriterReturnsSameWriterAndBlocksOutputStream() throws Exception {
		CompressionServletResponseWrapper wrapper = wrapperWritingTo(new CapturedServletOutputStream());

		PrintWriter writer = wrapper.getWriter();

		assertSame(writer, wrapper.getWriter());
		assertThrows(IllegalStateException.class, wrapper::getOutputStream);
	}

	@Test
	void setContentLengthIsIgnoredBecauseCompressedLengthIsUnknown() {
		HttpServletResponse response = mock(HttpServletResponse.class);
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(response);

		wrapper.setContentLength(123);

		verify(response, never()).setContentLength(123);
	}

	@Test
	void finishResponseClosesCreatedOutputStream() throws Exception {
		CapturedServletOutputStream output = new CapturedServletOutputStream();
		CompressionServletResponseWrapper wrapper = wrapperWritingTo(output);
		wrapper.setCompressionThreshold(8);
		wrapper.getOutputStream().write("abc".getBytes(java.nio.charset.StandardCharsets.UTF_8));

		wrapper.finishResponse();

		assertTrue(output.closed);
	}

	private static CompressionServletResponseWrapper wrapperWritingTo(ServletOutputStream output) throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getOutputStream()).thenReturn(output);
		when(response.getCharacterEncoding()).thenReturn(java.nio.charset.StandardCharsets.UTF_8.name());
		return new CompressionServletResponseWrapper(response);
	}

	private static final class CapturedServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		private boolean closed;

		@Override
		public void write(int b) {
			bytes.write(b);
		}

		@Override
		public void close() throws IOException {
			closed = true;
			super.close();
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// Synchronous test stream.
		}
	}
}
