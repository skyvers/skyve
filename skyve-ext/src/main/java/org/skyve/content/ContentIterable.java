package org.skyve.content;

import java.util.Iterator;

import jakarta.annotation.Nonnull;

/**
 * Iterable returned by ContentManager.all() to allow iterating over all content with resource handling.
 * @author mike
 */
public interface ContentIterable extends Iterable<SearchResult> {
	/**
	 * An iterator that can determine the total number of search hits.
	 * @author mike
	 */
	public interface ContentIterator extends Iterator<SearchResult> {
		public long getTotalHits();
	}

	@Override
	public @Nonnull ContentIterator iterator();
}
