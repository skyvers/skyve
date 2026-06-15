package org.skyve.impl.persistence.hibernate;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.ScrollableResults;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.LoggingIteratorAdapter;
import org.skyve.persistence.AutoClosingIterable;

/**
 * Auto-closing iterable that wraps a Hibernate scroll result set, closing the
 * underlying {@link org.hibernate.ScrollableResults} when the iterator is exhausted
 * or when {@link #close()} is explicitly called.
 */
public class HibernateAutoClosingIterable<T> implements AutoClosingIterable<T> {
	private String moduleName = null;
	private String documentName = null;
	@SuppressWarnings("resource") // Ownership is transferred to iterator() or released by close().
	private ScrollableResults results = null;
	private String[] aliases = null;
	boolean closed = false;
	boolean first = true;
	boolean assertSingle;
	boolean assertMultiple;
	
	/**
	 * Creates an iterable over raw Hibernate scroll results.
	 *
	 * @param results the scrollable results to consume
	 * @param assertSingle whether iteration should fail when a row contains more than one projected value
	 * @param assertMultiple whether iteration should fail when a row contains one or fewer projected values
	 */
	public HibernateAutoClosingIterable(ScrollableResults results, boolean assertSingle, boolean assertMultiple) {
		this(null, null, results, null, assertSingle, assertMultiple);
	}
	
	/**
	 * Creates an iterable that optionally projects each tuple into a dynamic bean.
	 *
	 * <p>When {@code moduleName} is non-null, each row is exposed as a
	 * {@link DynamicBean} keyed by {@code aliases}. Otherwise rows are returned as a
	 * scalar value or tuple array, depending on projection width.
	 *
	 * @param moduleName the module name for dynamic-bean projection, or {@code null} to return scalar or tuple values
	 * @param documentName the document name for dynamic-bean projection
	 * @param results the scrollable results to consume
	 * @param aliases the projected column aliases used when building dynamic beans
	 * @param assertSingle whether iteration should fail when a row contains more than one projected value
	 * @param assertMultiple whether iteration should fail when a row contains one or fewer projected values
	 */
	public HibernateAutoClosingIterable(String moduleName, 
											String documentName, 
											ScrollableResults results, 
											String[] aliases,
											boolean assertSingle,
											boolean assertMultiple) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.results = results;
		this.aliases = aliases;
		this.assertSingle = assertSingle;
		this.assertMultiple = assertMultiple;
	}

	private class HibernateIterator<Z> implements Iterator<Z> {
		@SuppressWarnings("hiding")
		private String moduleName = null;

		@SuppressWarnings("hiding")
		private String documentName = null;

		@SuppressWarnings({"hiding", "resource"}) // The iterator owns and closes these scroll results on exhaustion.
		private ScrollableResults results = null;

		@SuppressWarnings("hiding")
		private String[] aliases = null;

		private HibernateIterator(String moduleName, 
									String documentName, 
									ScrollableResults results, 
									String[] aliases) {
			this.moduleName = moduleName;
			this.documentName = documentName;
			this.results = results;
			this.aliases = aliases;
		}

		// This isn't exactly right because this hasNext() implementation has the side effect of moving on a record
		// This shouldn't matter because it should be used in an iterator for loop
		/**
		 * Advances the underlying cursor and reports whether another row is available.
		 *
		 * @return true when another row exists, otherwise false
		 */
		@Override
		public boolean hasNext() {
			boolean hasNext = results.next();

			if (! hasNext) {
				results.close();
				closed = true;
			}

			return hasNext;
		}

		/**
		 * Returns the current row as either a scalar, tuple, or DynamicBean projection.
		 *
		 * @return the projected row value for the current cursor position
		 */
		@Override
		@SuppressWarnings({"unchecked", "java:S2272"}) // hasNext() has side-effects
		public Z next() {
			Z result = null;

			Object[] tuple = results.get();
			if (first) {
				first = false;
				if (assertSingle && (tuple.length != 1)) {
					throw new IllegalStateException("There should be only 1 projected value in the query when using scalarIterable() or beanIterable()");
				}
				else if (assertMultiple && (tuple.length <= 1)) {
					throw new IllegalStateException("There should be more than 1 projected value in the query when using tupleIterable()");
				}
			}
			
			if (moduleName == null) {
				if (tuple.length == 1) {
					result = (Z) tuple[0];
				}
				else {
					result = (Z) tuple;
				}
			}
			else {
				Map<String, Object> properties = new TreeMap<>();

				int index = 0;
				while (index < aliases.length) {
					properties.put(aliases[index], tuple[index]);
					index++;
				}

				result = (Z) new DynamicBean(moduleName, documentName, properties);
			}

			return result;
		}

		/**
		 * This iterator does not support element removal.
		 *
		 * @throws IllegalAccessError always, because remove is not supported
		 */
		@Override
		public void remove() {
			throw new IllegalAccessError("Cannot remove from a HibernateBeanIterator.");
		}
	}

	/**
	 * Returns an iterator that closes the underlying scroll results once exhausted.
	 *
	 * <p>Side effects: transfers ownership of the current {@link ScrollableResults}
	 * instance to the returned iterator.
	 *
	 * @return an auto-closing iterator over the current results
	 */
	@Override
	public Iterator<T> iterator() {
		Iterator<T> i = new HibernateIterator<>(moduleName, documentName, results, aliases);

		results = null; // dereference the results

		return new LoggingIteratorAdapter<>(i);
	}

	/**
	 * Closes the underlying scroll results if they have not already been closed.
	 *
	 * @throws DomainException if Hibernate throws while closing the results
	 */
	@Override
	public void close() {
		try {
			if (! closed) {
				// results can be null on an error parsing the query
				if (results != null) {
					results.close();
				}
				closed = true;
			}
		}
		catch (Exception e) {
			throw new DomainException("Could not close the iterator.", e);
		}
	}
}
