package org.skyve.content;

import java.util.Iterator;

/**
 * Iterable returned by ContentManager.all() to allow iterating over all content with resource handling.
 * @author mike
 */
public interface ContentIterable extends Iterable<SearchResult> {
	/**
	 * An iterator that can report the number of search hits.
	 * @author mike
	 */
	public interface ContentIterator extends Iterator<SearchResult> {
		/**
		 * Returns the total number of hits, or a lower bound when counting them would mean reading every one.
		 *
		 * @return the total number of hits, or a lower bound on it
		 */
		public long getTotalHits();
	}

	@Override
	public ContentIterator iterator();
}
