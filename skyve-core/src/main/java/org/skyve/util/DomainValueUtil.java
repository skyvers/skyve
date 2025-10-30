package org.skyve.util;

import java.util.Comparator;

import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Utilities for working with Skyve Domain Values.
 * <p>
 * Provides comparators for sorting domain values by code or description.
 */
public class DomainValueUtil {
	/** comparator to allow sorting of domain values by code */
	public static class DomainValueSortByCode implements Comparator<DomainValue> {
		@Override
		public int compare(DomainValue d1, DomainValue d2) {
			return d1.getCode().compareTo(d2.getCode());
		}
	}

	/** comparator to allow sorting of domain values by description */
	public static class DomainValueSortByDescription implements Comparator<DomainValue> {
		@Override
		public int compare(DomainValue d1, DomainValue d2) {
			return d1.getLocalisedDescription().compareTo(d2.getLocalisedDescription());
		}
	}
}
