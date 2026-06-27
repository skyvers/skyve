package org.skyve.impl.web;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

import org.skyve.content.Disposition;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Writes a resolved download result to a servlet response.
 *
 * <p>Side effects: mutates response headers and writes the response body unless
 * {@code headOnly} is true. Stream lifecycle remains owned by the caller; this
 * class does not call {@link WebFileInputStream#processed()} or close stream-backed
 * downloads.
 *
 * <p>Range support is limited to file-backed downloads because they have a known
 * length and can be reopened from the start for each request. Byte-array downloads
 * preserve the historical buffered response behaviour, and arbitrary
 * {@link WebFileInputStream} downloads are streamed directly without range support
 * because their length and seek semantics are not guaranteed.
 *
 * <p>Parameters typed from the Servlet API are intentionally left unannotated
 * because those external contracts do not declare Skyve nullness constraints.
 */
final class DownloadResponseWriter {
	private DownloadResponseWriter() {
		// Utility class.
	}

	static void write(@Nonnull Download download,
						@Nullable String rangeHeader,
						@Nullable String ifRangeHeader,
						@Nonnull HttpServletResponse response,
						boolean headOnly)
	throws IOException {
		setDownloadHeaders(download, response);

		byte[] bytes = download.getBytes();
		if (bytes != null) {
			writeBytes(bytes, response, headOnly);
			return;
		}

		File file = download.getFile();
		if (file != null) {
			writeFile(file, rangeHeader, ifRangeHeader, response, headOnly);
			return;
		}

		@SuppressWarnings("resource") // The servlet owns the lifecycle and calls processed()/close() after writing.
		WebFileInputStream stream = download.getInputStream();
		if (stream != null) {
			writeStream(stream, response, headOnly);
			return;
		}

		response.setContentLength(0);
	}

	private static void setDownloadHeaders(@Nonnull Download download, @Nonnull HttpServletResponse response) {
		response.setContentType(download.getMimeType().toString());
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());

		StringBuilder header = new StringBuilder(64);
		Disposition disposition = download.getDisposition();
		header.append((disposition == null) ? Disposition.attachment.toString() : disposition.toString());
		header.append("; filename=\"").append(OWASP.sanitiseFileName(download.getFileName())).append('"');
		response.setHeader("Content-Disposition", header.toString());

		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
		response.setHeader("Cache-Control", "cache");
		response.setHeader("Pragma", "cache");
		response.addDateHeader("Expires", System.currentTimeMillis() + 60_000L);
	}

	private static void writeBytes(@Nonnull byte[] bytes, @Nonnull HttpServletResponse response, boolean headOnly)
	throws IOException {
		response.setContentLength(bytes.length);
		if (headOnly) {
			return;
		}

		@SuppressWarnings("resource") // The servlet container owns the response output stream.
		OutputStream out = response.getOutputStream();
		Util.chunkBytesToOutputStream(bytes, out);
		out.flush();
	}

	private static void writeFile(@Nonnull File file,
									@Nullable String rangeHeader,
									@Nullable String ifRangeHeader,
									@Nonnull HttpServletResponse response,
									boolean headOnly)
	throws IOException {
		long contentLength = file.length();
		long lastModified = file.lastModified();
		response.setHeader("Accept-Ranges", "bytes");
		if (lastModified > 0L) {
			response.setDateHeader("Last-Modified", lastModified);
		}

		HttpRange range = HttpRange.parse(rangeHeader, contentLength, ifRangeHeader, lastModified, response.getHeader("ETag"));
		if (range.isUnsatisfiable()) {
			response.setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
			response.setHeader("Content-Range", range.getContentRange());
			return;
		}

		if (range.isPartial()) {
			response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
			response.setHeader("Content-Range", range.getContentRange());
		}
		response.setContentLengthLong(range.getBytesToSend());
		if (headOnly) {
			return;
		}

		try (FileInputStream in = new FileInputStream(file)) {
			@SuppressWarnings("resource") // The servlet container owns the response output stream.
			OutputStream out = response.getOutputStream();
			StreamResponse.copy(in, out, range.getStart(), range.getBytesToSend());
			out.flush();
		}
	}

	private static void writeStream(@Nonnull WebFileInputStream stream, @Nonnull HttpServletResponse response, boolean headOnly)
	throws IOException {
		if (headOnly) {
			return;
		}

		@SuppressWarnings("resource") // The servlet container owns the response output stream.
		OutputStream out = response.getOutputStream();
		stream.transferTo(out);
		out.flush();
	}
}
