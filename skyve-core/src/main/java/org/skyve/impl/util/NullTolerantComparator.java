package org.skyve.impl.util;

import java.util.Comparator;

/**
 * A generic {@link Comparator} that sorts {@code null} values before any non-null
 * value, then delegates to the element's natural {@link Comparable} order.
 *
 * <p>Threading: stateless; safe for concurrent use.
 */
public final class NullTolerantComparator<T extends Comparable<T>> implements Comparator<T> {
	@Override
	public int compare(T t1, T t2) {
		int result = 0;

		if (t1 == null) {
			if (t2 != null) {
				result = -1;
			}
		}
		else {
			if (t2 == null) {
				result = 1;
			}
			else {
				result = ((Comparable<T>) t1).compareTo(t2);
			}
		}

		return result;
	}
}
