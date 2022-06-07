package org.skyve.persistence;

import java.util.Iterator;

import org.skyve.impl.util.LoggingIteratorAdapter;

/**
 * A class that adapts an Iterable (like many collections) for use in Skyve persistence.
 * @author mike
 *
 * @param <T>
 */
public class AutoClosingIterableAdpater<T> implements AutoClosingIterable<T> {
	private Iterable<T> iterable;
	
	public AutoClosingIterableAdpater(Iterable<T> adapted) {
		iterable = adapted;
	}
	
	@Override
	public Iterator<T> iterator() {
		return new LoggingIteratorAdapter<>(iterable.iterator());
	}

	@Override
	public void close() throws Exception {
		// nothing to do here
	}
}
