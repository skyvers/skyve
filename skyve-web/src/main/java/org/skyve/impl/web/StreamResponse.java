package org.skyve.impl.web;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import jakarta.annotation.Nonnull;

/**
 * Copies full or bounded response bodies without owning either stream.
 *
 * <p>Side effects: consumes bytes from the input stream and writes bytes to the
 * output stream. The caller owns both stream lifecycles and must close them when
 * appropriate.
 *
 * <p>Deferred scopes: thumbnail generation, dynamic image rendering, and reports
 * are intentionally buffered or generated elsewhere before bytes reach this helper.
 * Ranged copies are intended only for original, untransformed resources where the
 * caller has already validated security and resolved a trustworthy byte length.
 */
final class StreamResponse {
	/**
	 * Defines the reusable transfer buffer size for skip and copy operations.
	 */
	private static final int BUFFER_SIZE = 64 * 1024;

	/**
	 * Prevents instantiation of this utility class.
	 */
	private StreamResponse() {
		// Utility class.
	}

	/**
	 * Skips an initial range of bytes and writes the requested number of subsequent
	 * bytes to the output stream.
	 *
	 * <p>Precondition: {@code in} and {@code out} must be open and positioned for
	 * the requested operation. {@code bytesToSkip} and {@code bytesToCopy} must not
	 * be negative.
	 *
	 * <p>Side effects: advances {@code in} by {@code bytesToSkip + bytesToCopy}
	 * bytes and writes exactly {@code bytesToCopy} bytes to {@code out}. This method
	 * does not close or flush either stream.
	 *
	 * <p>Complexity: O(s + c) time where s is {@code bytesToSkip} and c is
	 * {@code bytesToCopy}; O(1) additional space.
	 *
	 * @param in the source stream; must not be {@code null}
	 * @param out the destination stream; must not be {@code null}
	 * @param bytesToSkip the number of leading bytes to discard
	 * @param bytesToCopy the number of bytes to write after the skipped range
	 * @throws IllegalArgumentException if either byte count is negative
	 * @throws EOFException if {@code in} ends before the requested skip or copy
	 *             completes
	 * @throws IOException if either stream reports an I/O failure
	 */
	static void copy(@Nonnull InputStream in, @Nonnull OutputStream out, long bytesToSkip, long bytesToCopy)
	throws IOException {
		if (bytesToSkip < 0) {
			throw new IllegalArgumentException("bytesToSkip must not be negative");
		}
		if (bytesToCopy < 0) {
			throw new IllegalArgumentException("bytesToCopy must not be negative");
		}

		byte[] buffer = new byte[BUFFER_SIZE];
		skipFully(in, buffer, bytesToSkip);
		copyFully(in, out, buffer, bytesToCopy);
	}

	/**
	 * Advances the input stream by the requested number of bytes, reading into the
	 * supplied buffer when the stream cannot skip directly.
	 *
	 * <p>Precondition: {@code buffer} must have positive length so fallback reads can
	 * make progress when {@link InputStream#skip(long)} returns zero.
	 *
	 * <p>Side effects: consumes exactly {@code bytesToSkip} bytes from {@code in}.
	 *
	 * @param in the source stream; must not be {@code null}
	 * @param buffer the reusable scratch buffer; must not be {@code null} or empty
	 * @param bytesToSkip the non-negative number of bytes to discard
	 * @throws EOFException if {@code in} ends before the requested offset is reached
	 * @throws IOException if {@code in} reports an I/O failure
	 */
	private static void skipFully(@Nonnull InputStream in, @Nonnull byte[] buffer, long bytesToSkip)
	throws IOException {
		long remaining = bytesToSkip;
		while (remaining > 0) {
			long skipped = in.skip(remaining);
			if (skipped > 0) {
				remaining -= skipped;
			}
			else {
				int length = (int) Math.min(buffer.length, remaining);
				int read = in.read(buffer, 0, length);
				if (read < 0) {
					throw new EOFException("Input stream ended before the requested offset");
				}
				remaining -= read;
			}
		}
	}

	/**
	 * Writes the requested number of bytes from the input stream to the output
	 * stream using the supplied transfer buffer.
	 *
	 * <p>Side effects: consumes exactly {@code bytesToCopy} bytes from {@code in}
	 * and writes the same bytes to {@code out}. This method does not close or flush
	 * either stream.
	 *
	 * @param in the source stream; must not be {@code null}
	 * @param out the destination stream; must not be {@code null}
	 * @param buffer the reusable transfer buffer; must not be {@code null} or empty
	 * @param bytesToCopy the non-negative number of bytes to copy
	 * @throws EOFException if {@code in} ends before the requested byte count is
	 *             copied
	 * @throws IOException if either stream reports an I/O failure
	 */
	private static void copyFully(@Nonnull InputStream in, @Nonnull OutputStream out, @Nonnull byte[] buffer, long bytesToCopy)
	throws IOException {
		long remaining = bytesToCopy;
		while (remaining > 0) {
			int length = (int) Math.min(buffer.length, remaining);
			int read = in.read(buffer, 0, length);
			if (read < 0) {
				throw new EOFException("Input stream ended before the requested byte count was copied");
			}
			out.write(buffer, 0, read);
			remaining -= read;
		}
	}
}
