package org.skyve.impl.util;

import java.util.Iterator;

import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * An iterator adapter that logs a diagnostic message when iteration completes after
 * at least 1 000 elements, helping to identify unexpectedly large result sets.
 *
 * <p>Delegates all iterator operations to the wrapped {@code Iterator<T>}. The log
 * message is emitted at INFO level via the Skyve logging infrastructure.
 */
public class LoggingIteratorAdapter<T> implements Iterator<T> {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(LoggingIteratorAdapter.class);

	private Iterator<T> adapted;
	private int iterated = 0;

	public LoggingIteratorAdapter(final @Nonnull Iterator<T> adapted) {
		this.adapted = adapted;
	}

	@Override
	public boolean hasNext() {
		boolean result = adapted.hasNext();
		if ((! result) && (iterated >= 1000)) {
			LOGGER.info("Finished Iteration at {} iterations", iterated);
		}
		return result;
	}

	@Override
	public @Nullable T next() {
		T result = adapted.next();
		iterated++;
		if (iterated % 1000 == 0) {
			LOGGER.info("Iterated {}", iterated);
		}
		return result;
	}

}
