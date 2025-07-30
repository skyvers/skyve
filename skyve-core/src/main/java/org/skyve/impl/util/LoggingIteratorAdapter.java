package org.skyve.impl.util;

import java.util.Iterator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggingIteratorAdapter<T> implements Iterator<T> {
    private static final Logger LOGGER = LoggerFactory.getLogger(LoggingIteratorAdapter.class);

	private Iterator<T> adapted;
	private int iterated = 0;

	public LoggingIteratorAdapter(final Iterator<T> adapted) {
		this.adapted = adapted;
	}

	@Override
	public boolean hasNext() {
		boolean result = adapted.hasNext();
		if ((! result) && (iterated >= 1000)) {
			LOGGER.info("Finished Iteration at " + iterated + " iterations");
		}
		return result;
	}

	@Override
	public T next() {
		T result = adapted.next();
		iterated++;
		if (iterated % 1000 == 0) {
			LOGGER.info("Iterated " + iterated);
		}
		return result;
	}

}
