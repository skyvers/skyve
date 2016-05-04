package org.skyve.content;

import java.util.Iterator;

public interface ContentIterable extends Iterable<SearchResult> {
	public interface ContentIterator extends Iterator<SearchResult> {
		public long getTotalHits();
	}

	@Override
	public ContentIterator iterator();
}
