package org.skyve.persistence;

import java.util.Iterator;

import org.skyve.impl.util.LoggingIteratorAdapter;

/**
 * Adapts any {@link Iterable} (such as a {@link java.util.Collection}) to the
 * {@link AutoClosingIterable} contract.
 *
 * <p>The adapted iterable is wrapped in a {@link org.skyve.impl.util.LoggingIteratorAdapter}
 * for diagnostic purposes. {@link #close()} is a no-op because in-memory iterables hold\n * no database resources.
 *
 * <p>Use this to pass a pre-fetched collection to an API that expects an
 * {@link AutoClosingIterable}, avoiding the overhead of an additional database query.
 *
 * @param <T> the element type
 */
public class AutoClosingIterableAdpater<T> implements AutoClosingIterable<T> {
	private Iterable<T> iterable;
	
	/**
	 * Creates a new AutoClosingIterableAdpater instance.
	 * @param adapted the adapted
	 */
	public AutoClosingIterableAdpater(Iterable<T> adapted) {
		iterable = adapted;
	}
	
	/**
	 * Executes iterator.
	 * @return the result
	 */
	@Override
	public Iterator<T> iterator() {
		return new LoggingIteratorAdapter<>(iterable.iterator());
	}

	/**
	 * Executes close.
	 */
	@Override
	public void close() throws Exception {
		// nothing to do here
	}
}
