package org.skyve.impl.util;

import java.util.Iterator;

import org.skyve.util.Util;

public class LoggingIteratorAdapter<T> implements Iterator<T> {
	private Iterator<T> adapted;
	private int iterated = 0;

	public LoggingIteratorAdapter(final Iterator<T> adapted) {
		this.adapted = adapted;
	}

	@Override
	public boolean hasNext() {
		boolean result = adapted.hasNext();
		if ((! result) && (iterated >= 1000)) {
			Util.LOGGER.info("Finished Iteration at " + iterated + " iterations");
		}
		return result;
	}

	@Override
	public T next() {
		T result = adapted.next();
		iterated++;
		if (iterated % 1000 == 0) {
			Util.LOGGER.info("Iterated " + iterated);
		}
		return result;
	}

}
