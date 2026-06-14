package org.skyve.impl.web.filter.gzip;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.zip.GZIPInputStream;

import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource", "boxing"})
class CompressionResponseStreamTest {
	@Test
	void closeWritesBufferedBytesWithoutGzipWhenThresholdNotReached() throws Exception {
		CapturedServletOutputStream output = new CapturedServletOutputStream();
		HttpServletResponse response = responseWritingTo(output, false);
		CompressionResponseStream stream = new CompressionResponseStream(response);
		stream.setBuffer(8);

		stream.write("abc".getBytes(StandardCharsets.UTF_8));
		stream.close();

		assertArrayEquals("abc".getBytes(StandardCharsets.UTF_8), output.bytes());
		assertTrue(stream.closed());
		verify(response, never()).addHeader("Content-Encoding", "gzip");
	}

	@Test
	void writeCompressesBytesWhenBufferThresholdIsExceeded() throws Exception {
		CapturedServletOutputStream output = new CapturedServletOutputStream();
		HttpServletResponse response = responseWritingTo(output, false);
		CompressionResponseStream stream = new CompressionResponseStream(response);
		stream.setBuffer(4);

		stream.write("abcdef".getBytes(StandardCharsets.UTF_8));
		stream.close();

		assertArrayEquals("abcdef".getBytes(StandardCharsets.UTF_8), gunzip(output.bytes()));
		verify(response).addHeader("Content-Encoding", "gzip");
	}

	@Test
	void committedResponseUsesOriginalOutputStreamInsteadOfGzip() throws Exception {
		CapturedServletOutputStream output = new CapturedServletOutputStream();
		HttpServletResponse response = responseWritingTo(output, true);
		CompressionResponseStream stream = new CompressionResponseStream(response);
		stream.setBuffer(2);

		stream.write("abcd".getBytes(StandardCharsets.UTF_8));
		stream.close();

		assertArrayEquals("abcd".getBytes(StandardCharsets.UTF_8), output.bytes());
		verify(response, never()).addHeader("Content-Encoding", "gzip");
	}

	@Test
	void closedStreamRejectsFurtherFlushAndWrite() throws Exception {
		CapturedServletOutputStream output = new CapturedServletOutputStream();
		CompressionResponseStream stream = new CompressionResponseStream(responseWritingTo(output, false));
		stream.setBuffer(4);

		stream.close();

		assertThrows(IOException.class, stream::flush);
		assertThrows(IOException.class, () -> stream.write(1));
		assertTrue(output.isReady());
	}

	private static HttpServletResponse responseWritingTo(ServletOutputStream output, boolean committed) throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getOutputStream()).thenReturn(output);
		when(response.isCommitted()).thenReturn(Boolean.valueOf(committed));
		return response;
	}

	private static byte[] gunzip(byte[] bytes) throws IOException {
		try (GZIPInputStream gzip = new GZIPInputStream(new ByteArrayInputStream(bytes))) {
			return gzip.readAllBytes();
		}
	}

	private static final class CapturedServletOutputStream extends ServletOutputStream {
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

		private byte[] bytes() {
			return bytes.toByteArray();
		}
	}
}
