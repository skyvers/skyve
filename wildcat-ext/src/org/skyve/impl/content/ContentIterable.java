package org.skyve.impl.content;

import java.util.Iterator;

import org.skyve.impl.content.SearchResult;

public interface ContentIterable extends Iterable<SearchResult> {
	public interface ContentIterator extends Iterator<SearchResult> {
		public long getTotalHits();
	}

	@Override
	public ContentIterator iterator();
}
