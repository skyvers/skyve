package org.skyve.wildcat.util;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import org.hibernate.ScrollableResults;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.AutoClosingBeanIterable;
import org.skyve.wildcat.domain.MapBean;

public class HibernateAutoClosingBeanIterable<T extends Bean> implements AutoClosingBeanIterable<T> {
	private String moduleName = null;
	private String documentName = null;
	private ScrollableResults results = null;
	private String[] aliases = null;
	boolean closed = false;

	public HibernateAutoClosingBeanIterable(String moduleName, 
									String documentName, 
									ScrollableResults results, 
									String[] aliases) {
		this.moduleName = moduleName;
		this.documentName = documentName;
		this.results = results;
		this.aliases = aliases;
	}

	private class HibernateBeanIterator<Z extends Bean> implements Iterator<Z> {
		@SuppressWarnings("hiding")
		private String moduleName = null;

		@SuppressWarnings("hiding")
		private String documentName = null;

		@SuppressWarnings("hiding")
		private ScrollableResults results = null;

		@SuppressWarnings("hiding")
		private String[] aliases = null;

		private HibernateBeanIterator(String moduleName, 
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

			if ((tuple.length == 1) && (tuple[0] instanceof Bean)) {
				result = (Z) tuple[0];
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
		Iterator<T> i = new HibernateBeanIterator<>(moduleName, documentName, results, aliases);

		results = null; // dereference the results

		return i;
	}

	@Override
	public void close() throws DomainException {
		try {
			if (! closed) {
				results.close();
				closed = true;
			}
		}
		catch (Exception e) {
			throw new DomainException("Could not close the iterator.", e);
		}
	}
}
