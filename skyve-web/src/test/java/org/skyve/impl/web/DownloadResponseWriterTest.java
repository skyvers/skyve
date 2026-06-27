package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.content.Disposition;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.WebFileInputStream;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class DownloadResponseWriterTest {
	@TempDir
	private Path tempDir;

	@Test
	void byteBackedDownloadPreservesHeadersAndWritesBytes() throws Exception {
		byte[] bytes = "download bytes".getBytes(StandardCharsets.UTF_8);
		Download download = new Download("hello.txt", bytes, MimeType.plain, Disposition.attachment);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=2-5", null, response, false);

		verifyStandardHeaders(response, MimeType.plain, "attachment; filename=\"hello.txt\"");
		verify(response).setContentLength(bytes.length);
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		assertEquals("download bytes", out.asString());
	}

	@Test
	void byteBackedHeadOnlyDownloadSetsLengthWithoutBody() throws Exception {
		byte[] bytes = "download bytes".getBytes(StandardCharsets.UTF_8);
		Download download = new Download("hello.txt", bytes, MimeType.plain, Disposition.attachment);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, null, null, response, true);

		verify(response).setContentLength(bytes.length);
		verify(response, never()).getOutputStream();
	}

	@Test
	void nullDispositionDefaultsToAttachmentAndFilenameIsSanitised() throws Exception {
		byte[] bytes = "safe".getBytes(StandardCharsets.UTF_8);
		Download download = new Download("../unsafe<name>.txt", bytes, MimeType.plain, null);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, null, null, response, false);

		verify(response).setHeader("Content-Disposition", "attachment; filename=\"..unsafename.txt\"");
		assertEquals("safe", out.asString());
	}

	@Test
	void fileBackedDownloadStreamsFullResponseAndSetsLength() throws Exception {
		Path file = fileWithContent("full.txt", "abcdef");
		Download download = new Download("full.txt", file.toFile(), MimeType.octetStream, Disposition.attachment);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, null, null, response, false);

		verifyStandardHeaders(response, MimeType.octetStream, "attachment; filename=\"full.txt\"");
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentLengthLong(6L);
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		assertEquals("abcdef", out.asString());
	}

	@Test
	void fileBackedDownloadWritesValidRange() throws Exception {
		Path file = fileWithContent("range.bin", "abcdef");
		Download download = new Download("range.bin", file.toFile(), MimeType.octetStream, Disposition.inline);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=2-4", null, response, false);

		verifyStandardHeaders(response, MimeType.octetStream, "inline; filename=\"range.bin\"");
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Content-Range", "bytes 2-4/6");
		verify(response).setContentLengthLong(3L);
		assertEquals("cde", out.asString());
	}

	@Test
	void fileBackedDownloadHonoursRangeWhenIfRangeDateMatches() throws Exception {
		Path file = fileWithContent("if-range-match.bin", "abcdef");
		long lastModified = 1_700_000_000_000L;
		file.toFile().setLastModified(lastModified);
		Download download = new Download("if-range-match.bin", file.toFile(), MimeType.octetStream);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=1-3", httpDate(lastModified), response, false);

		verify(response).setDateHeader("Last-Modified", lastModified);
		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Content-Range", "bytes 1-3/6");
		verify(response).setContentLengthLong(3L);
		assertEquals("bcd", out.asString());
	}

	@Test
	void fileBackedDownloadIgnoresRangeWhenIfRangeDateIsStale() throws Exception {
		Path file = fileWithContent("if-range-stale.bin", "abcdef");
		long lastModified = 1_700_000_000_000L;
		file.toFile().setLastModified(lastModified);
		Download download = new Download("if-range-stale.bin", file.toFile(), MimeType.octetStream);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=1-3", httpDate(lastModified - 1_000L), response, false);

		verify(response).setDateHeader("Last-Modified", lastModified);
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response, never()).setHeader(org.mockito.ArgumentMatchers.eq("Content-Range"), org.mockito.ArgumentMatchers.anyString());
		verify(response).setContentLengthLong(6L);
		assertEquals("abcdef", out.asString());
	}

	@Test
	void fileBackedHeadOnlyDownloadOmitsLastModifiedWhenFileTimestampIsUnknown() throws Exception {
		Download download = new Download("unknown-time.bin", new UnknownLastModifiedFile("unknown-time.bin", 6L), MimeType.octetStream);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, null, null, response, true);

		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setDateHeader(eq("Last-Modified"), anyLong());
		verify(response).setContentLengthLong(6L);
		verify(response, never()).getOutputStream();
	}

	@Test
	void fileBackedUnsatisfiableRangeReturns416WithoutBody() throws Exception {
		Path file = fileWithContent("unsatisfiable.bin", "abcdef");
		Download download = new Download("unsatisfiable.bin", file.toFile(), MimeType.octetStream, Disposition.attachment);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, "bytes=99-", null, response, false);

		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
		verify(response).setHeader("Content-Range", "bytes */6");
		verify(response, never()).getOutputStream();
		verify(response, never()).setContentLengthLong(anyLong());
	}

	@Test
	void malformedRangeFallsBackToFullFileResponse() throws Exception {
		Path file = fileWithContent("malformed.bin", "abcdef");
		Download download = new Download("malformed.bin", file.toFile(), MimeType.octetStream);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=bad", null, response, false);

		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentLengthLong(6L);
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response, never()).setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
		assertEquals("abcdef", out.asString());
	}

	@Test
	void multipleRangesFallBackToFullFileResponse() throws Exception {
		Path file = fileWithContent("multiple.bin", "abcdef");
		Download download = new Download("multiple.bin", file.toFile(), MimeType.octetStream);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=0-1,3-4", null, response, false);

		verify(response).setContentLengthLong(6L);
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		assertEquals("abcdef", out.asString());
	}

	@Test
	void streamBackedDownloadWritesDirectlyWithoutRangeOrLengthHeaders() throws Exception {
		TrackingInputStream raw = new TrackingInputStream("stream bytes".getBytes(StandardCharsets.UTF_8));
		WebFileInputStream stream = new WebFileInputStream(raw);
		Download download = new Download("stream.txt", stream, MimeType.plain, Disposition.attachment);
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		DownloadResponseWriter.write(download, "bytes=1-4", null, response, false);

		verifyStandardHeaders(response, MimeType.plain, "attachment; filename=\"stream.txt\"");
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setContentLength(anyInt());
		verify(response, never()).setContentLengthLong(anyLong());
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		assertEquals("stream bytes", out.asString());
		assertFalse(raw.closed);
	}

	@Test
	void streamBackedHeadOnlyDownloadSetsHeadersWithoutReadingStream() throws Exception {
		TrackingInputStream raw = new TrackingInputStream("stream bytes".getBytes(StandardCharsets.UTF_8));
		WebFileInputStream stream = new WebFileInputStream(raw);
		Download download = new Download("stream.txt", stream, MimeType.plain, Disposition.attachment);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, "bytes=1-4", null, response, true);

		verifyStandardHeaders(response, MimeType.plain, "attachment; filename=\"stream.txt\"");
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setContentLength(anyInt());
		verify(response, never()).setContentLengthLong(anyLong());
		verify(response, never()).getOutputStream();
		assertEquals(0, raw.readCount);
		assertFalse(raw.closed);
	}

	@Test
	void emptyDownloadSetsZeroLength() throws Exception {
		Download download = org.mockito.Mockito.mock(Download.class);
		org.mockito.Mockito.when(download.getMimeType()).thenReturn(MimeType.plain);
		org.mockito.Mockito.when(download.getFileName()).thenReturn("empty.txt");
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, null, null, response, false);

		verifyStandardHeaders(response, MimeType.plain, "attachment; filename=\"empty.txt\"");
		verify(response).setContentLength(0);
		verify(response, never()).getOutputStream();
	}

	@Test
	void headOnlyFileDownloadSetsHeadersAndWritesNoBody() throws Exception {
		Path file = fileWithContent("head.bin", "abcdef");
		Download download = new Download("head.bin", file.toFile(), MimeType.octetStream);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		DownloadResponseWriter.write(download, "bytes=1-3", null, response, true);

		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Content-Range", "bytes 1-3/6");
		verify(response).setContentLengthLong(3L);
		verify(response, never()).getOutputStream();
	}

	private Path fileWithContent(String name, String content) throws IOException {
		Path file = tempDir.resolve(name);
		Files.writeString(file, content, StandardCharsets.UTF_8);
		return file;
	}

	private static String httpDate(long epochMillis) {
		return DateTimeFormatter.RFC_1123_DATE_TIME.format(Instant.ofEpochMilli(epochMillis).atZone(ZoneOffset.UTC));
	}

	private static HttpServletResponse responseWithOutput(CapturingServletOutputStream out) throws IOException {
		HttpServletResponse response = org.mockito.Mockito.mock(HttpServletResponse.class);
		doReturn(out).when(response).getOutputStream();
		return response;
	}

	private static void verifyStandardHeaders(HttpServletResponse response, MimeType mimeType, String disposition) {
		verify(response).setContentType(mimeType.toString());
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response).setHeader("Content-Disposition", disposition);
		verify(response).setHeader("Cache-Control", "cache");
		verify(response).setHeader("Pragma", "cache");
		verify(response).addDateHeader(eq("Expires"), anyLong());
	}

	private static final class TrackingInputStream extends ByteArrayInputStream {
		private boolean closed;
		private int readCount;

		private TrackingInputStream(byte[] bytes) {
			super(bytes);
		}

		@Override
		public void close() throws IOException {
			closed = true;
			super.close();
		}

		@Override
		public synchronized int read(byte[] b, int off, int len) {
			readCount++;
			return super.read(b, off, len);
		}

		@Override
		public synchronized int read() {
			readCount++;
			return super.read();
		}
	}

	private static final class UnknownLastModifiedFile extends java.io.File {
		private static final long serialVersionUID = 1L;

		private final long length;

		private UnknownLastModifiedFile(String pathname, long length) {
			super(pathname);
			this.length = length;
		}

		@Override
		public long length() {
			return length;
		}

		@Override
		public long lastModified() {
			return 0L;
		}
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
