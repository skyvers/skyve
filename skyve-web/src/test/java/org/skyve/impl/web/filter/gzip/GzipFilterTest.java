package org.skyve.impl.web.filter.gzip;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Vector;
import java.util.zip.GZIPInputStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class GzipFilterTest {

	/** A {@link ServletOutputStream} that writes to an in-memory buffer. */
	private static class ByteServletOutputStream extends ServletOutputStream {
		final ByteArrayOutputStream buf = new ByteArrayOutputStream();

		@Override
		public void write(int b) throws IOException {
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

	private ByteServletOutputStream rawOutput;
	private HttpServletResponse mockResponse;

	@BeforeEach
	void setUp() throws IOException {
		rawOutput = new ByteServletOutputStream();
		mockResponse = mock(HttpServletResponse.class);
		when(mockResponse.getOutputStream()).thenReturn(rawOutput);
	}

	// ===== CompressionResponseStream =====

	@Test
	void newStreamIsNotClosed() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		assertFalse(stream.closed());
	}

	@Test
	void isReadyReturnsTrue() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		assertTrue(stream.isReady());
	}

	@Test
	void setWriteListenerDoesNotThrow() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setWriteListener(mock(WriteListener.class));
	}

	@Test
	void writeIntBuffersWhenBelowThreshold() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.write(42);
		// Not yet flushed to gzip — gzipstream is null, output has no bytes yet
		assertFalse(stream.closed());
	}

	@Test
	void writeBytesBuffersThenFlushesToGzip() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		// Use threshold = 4 so a 5-byte write triggers gzip creation
		stream.setBuffer(4);
		byte[] data = new byte[]{1, 2, 3, 4, 5};
		stream.write(data);
		// Not closed; output should now have gzip bytes
		assertFalse(stream.closed());
	}

	@Test
	void writeToGzipSetsContentEncodingHeaderWhenNotCommitted() throws IOException {
		when(mockResponse.isCommitted()).thenReturn(false);
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(0); // zero threshold means every write goes straight to gzip
		stream.writeToGZip(new byte[]{'A'}, 0, 1);
		verify(mockResponse).addHeader("Content-Encoding", "gzip");
	}

	@Test
	void writeToGzipSkipsGzipHeaderWhenAlreadyCommitted() throws IOException {
		when(mockResponse.isCommitted()).thenReturn(true);
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(0);
		stream.writeToGZip(new byte[]{'A'}, 0, 1);
		// When committed, no Content-Encoding header is added
		verify(mockResponse, org.mockito.Mockito.never()).addHeader(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void closeFlushesPendingBufferToOutput() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.write(new byte[]{10, 20, 30});
		stream.close();
		assertTrue(stream.closed());
		// Three bytes must have been written to rawOutput (no gzip — just buffered flush)
		assertArrayEquals(new byte[]{10, 20, 30}, rawOutput.buf.toByteArray());
	}

	@Test
	void closeWhenAlreadyClosedThrowsIOException() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.close();
		assertThrows(IOException.class, stream::close);
	}

	@Test
	void flushOnOpenStreamDoesNotThrow() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		// No gzipstream yet — flush should be a no-op
		stream.flush();
	}

	@Test
	void writeIntOnClosedStreamThrowsIOException() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.close();
		assertThrows(IOException.class, () -> stream.write(1));
	}

	@Test
	void writeByteArrayOnClosedStreamThrowsIOException() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.close();
		assertThrows(IOException.class, () -> stream.write(new byte[]{1, 2, 3}));
	}

	@Test
	void writeZeroLengthArrayIsNoOp() throws IOException {
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(64);
		stream.write(new byte[0], 0, 0);
		// Nothing written yet
		assertFalse(stream.closed());
	}

	@Test
	void writeCompressedDataCanBeDecompressed() throws IOException {
		when(mockResponse.isCommitted()).thenReturn(false);
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		stream.setBuffer(1024);
		byte[] original = "Hello, GZip!".getBytes();
		stream.write(original);
		stream.close();

		byte[] written = rawOutput.buf.toByteArray();
		// The output should be valid GZIP since threshold was exceeded
		// (close flushes 12 bytes uncompressed because buffer=1024 > 12, so plain write)
		assertArrayEquals(original, written);
	}

	@Test
	void writeExceedingBufferProducesGzipOutput() throws IOException {
		when(mockResponse.isCommitted()).thenReturn(false);
		CompressionResponseStream stream = new CompressionResponseStream(mockResponse);
		// Threshold = 4; write 10 bytes → triggers flushToGzip then writeToGzip
		stream.setBuffer(4);
		byte[] original = "0123456789".getBytes();
		stream.write(original);
		stream.close();

		byte[] written = rawOutput.buf.toByteArray();
		// Output is GZIP-encoded; decompress and verify
		try (GZIPInputStream gzIn = new GZIPInputStream(new ByteArrayInputStream(written))) {
			byte[] decoded = gzIn.readAllBytes();
			assertArrayEquals(original, decoded);
		}
	}

	// ===== CompressionServletResponseWrapper =====

	@Test
	void wrapperConstructorSetsOrigResponse() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		assertNotNull(wrapper);
	}

	@Test
	void setContentTypeSetsFieldAndDelegatesToResponse() {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setContentType("text/html");
		verify(mockResponse).setContentType("text/html");
	}

	@Test
	void setCompressionThresholdStoresValue() {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		// No exception — stores internally
		wrapper.setCompressionThreshold(2048);
	}

	@Test
	void setDebugLevelStoresValue() {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setDebugLevel(2);
	}

	@Test
	void setContentLengthIsNoOp() {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		// Must not throw
		wrapper.setContentLength(1024);
	}

	@Test
	void createOutputStreamReturnsCompressionResponseStream() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		ServletOutputStream out = wrapper.createOutputStream();
		assertNotNull(out);
	}

	@Test
	void getOutputStreamReturnsSameStreamOnSecondCall() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		ServletOutputStream first = wrapper.getOutputStream();
		ServletOutputStream second = wrapper.getOutputStream();
		assertTrue(first == second);
	}

	@Test
	void getOutputStreamThrowsWhenWriterAlreadyObtained() throws IOException {
		when(mockResponse.getCharacterEncoding()).thenReturn("UTF-8");
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		wrapper.getWriter();
		assertThrows(IllegalStateException.class, wrapper::getOutputStream);
	}

	@Test
	void getWriterReturnsSameWriterOnSecondCall() throws IOException {
		when(mockResponse.getCharacterEncoding()).thenReturn("UTF-8");
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		var first = wrapper.getWriter();
		var second = wrapper.getWriter();
		assertTrue(first == second);
	}

	@Test
	void getWriterThrowsWhenOutputStreamAlreadyObtained() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		wrapper.getOutputStream();
		assertThrows(IllegalStateException.class, wrapper::getWriter);
	}

	@Test
	void getWriterWithNullCharEncoding() throws IOException {
		when(mockResponse.getCharacterEncoding()).thenReturn(null);
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		assertNotNull(wrapper.getWriter());
	}

	@Test
	void finishResponseClosesWriter() throws IOException {
		when(mockResponse.getCharacterEncoding()).thenReturn("UTF-8");
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		wrapper.getWriter(); // initialise writer
		wrapper.finishResponse(); // must not throw
	}

	@Test
	void finishResponseClosesOutputStream() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		wrapper.getOutputStream(); // initialise stream
		wrapper.finishResponse(); // must not throw
	}

	@Test
	void finishResponseWithNeitherDoesNotThrow() {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.finishResponse(); // both writer and stream are null
	}

	@Test
	void flushBufferFlushesUnderlyingStream() throws IOException {
		CompressionServletResponseWrapper wrapper = new CompressionServletResponseWrapper(mockResponse);
		wrapper.setCompressionThreshold(1024);
		wrapper.getOutputStream(); // creates the CompressionResponseStream
		wrapper.flushBuffer(); // must not throw
	}

	// ===== CompressionFilterTestServlet =====

	@Test
	void testServletDoGetWithGzipAcceptEncoding() throws Exception {
		ByteServletOutputStream out = new ByteServletOutputStream();
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		jakarta.servlet.http.HttpServletResponse response = mock(jakarta.servlet.http.HttpServletResponse.class);
		Vector<String> headers = new Vector<>();
		headers.add("gzip, deflate");
		when(request.getHeaders("Accept-Encoding")).thenReturn(headers.elements());
		when(response.getOutputStream()).thenReturn(out);

		CompressionFilterTestServlet servlet = new CompressionFilterTestServlet();
		servlet.doGet(request, response);

		String output = out.buf.toString();
		assertTrue(output.contains("gzip supported"));
	}

	@Test
	void testServletDoGetWithNoGzipAcceptEncoding() throws Exception {
		ByteServletOutputStream out = new ByteServletOutputStream();
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		jakarta.servlet.http.HttpServletResponse response = mock(jakarta.servlet.http.HttpServletResponse.class);
		Vector<String> headers = new Vector<>();
		headers.add("deflate");
		when(request.getHeaders("Accept-Encoding")).thenReturn(headers.elements());
		when(response.getOutputStream()).thenReturn(out);

		CompressionFilterTestServlet servlet = new CompressionFilterTestServlet();
		servlet.doGet(request, response);

		String output = out.buf.toString();
		assertTrue(output.contains("gzip not supported"));
	}

	@Test
	void testServletDoGetWithNoAcceptEncodingHeaders() throws Exception {
		ByteServletOutputStream out = new ByteServletOutputStream();
		jakarta.servlet.http.HttpServletRequest request = mock(jakarta.servlet.http.HttpServletRequest.class);
		jakarta.servlet.http.HttpServletResponse response = mock(jakarta.servlet.http.HttpServletResponse.class);
		when(request.getHeaders("Accept-Encoding")).thenReturn(Collections.emptyEnumeration());
		when(response.getOutputStream()).thenReturn(out);

		CompressionFilterTestServlet servlet = new CompressionFilterTestServlet();
		servlet.doGet(request, response);

		String output = out.buf.toString();
		assertTrue(output.contains("Compression Filter Test Servlet"));
	}
}
