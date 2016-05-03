package org.skyve.impl.util;

import java.util.Comparator;

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
