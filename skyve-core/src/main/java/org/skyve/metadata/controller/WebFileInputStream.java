package org.skyve.metadata.controller;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Defers physical stream closure until Skyve marks the stream as processed.
 *
 * <p>This wrapper is used by upload/download pipelines where stream ownership
 * spans multiple processing stages. Calling {@link #close()} before
 * {@link #processed()} is a no-op; once processed, {@code close()} delegates to
 * the wrapped stream.
 *
 * <p>Threading: safe for simple cross-thread visibility of the processed flag
 * via {@code volatile}; broader stream usage remains thread-confined unless the
 * wrapped stream itself is thread-safe.
 */
public class WebFileInputStream extends FilterInputStream {
	private volatile boolean processed = false;
	
	/**
	 * Creates a new WebFileInputStream instance.
	 * @param in the in
	 */
	public WebFileInputStream(InputStream in) {
		super(in);
	}
	
	/**
	 * Executes processed.
	 */
	public void processed() {
		processed = true;
	}

	/**
	 * Executes close.
	 */
	@Override
	public void close() throws IOException {
		if (processed) {
			super.close();
		}
	}
}
