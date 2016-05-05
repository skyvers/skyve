package org.skyve.impl.persistence.hibernate;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.ScrollableResults;
import org.skyve.domain.MapBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.AutoClosingIterable;

public class HibernateAutoClosingIterable<T> implements AutoClosingIterable<T> {
	private String moduleName = null;
	private String documentName = null;
	private ScrollableResults results = null;
	private String[] aliases = null;
	boolean closed = false;
	boolean first = true;
	boolean assertSingle;
	boolean assertMultiple;
	
	public HibernateAutoClosingIterable(ScrollableResults results, boolean assertSingle, boolean assertMultiple) {
		this(null, null, results, null, assertSingle, assertMultiple);
	}
	
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

		@SuppressWarnings("hiding")
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
		@Override
		public boolean hasNext() {
			boolean hasNext = results.next();

			if (! hasNext) {
				results.close();
				closed = true;
			}

			return hasNext;
		}

		@Override
		@SuppressWarnings("unchecked")
		public Z next() {
			Z result = null;

			Object[] tuple = results.get();
			if (first) {
				first = false;
				if (assertSingle && (tuple.length != 1)) {
					throw new IllegalStateException("There should be only 1 projected value in the query");
				}
				else if (assertMultiple && (tuple.length <= 1)) {
					throw new IllegalStateException("There should be more than 1 projected value in the query");
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

				result = (Z) new MapBean(moduleName, documentName, properties);
			}

			return result;
		}

		@Override
		public void remove() {
			throw new IllegalAccessError("Cannot remove from a HibernateBeanIterator.");
		}
	}

	@Override
	@SuppressWarnings("synthetic-access")
	public Iterator<T> iterator() {
		Iterator<T> i = new HibernateIterator<>(moduleName, documentName, results, aliases);

		results = null; // dereference the results

		return i;
	}

	@Override
	public void close() throws DomainException {
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
