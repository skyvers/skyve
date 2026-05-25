package org.skyve.metadata;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Specifies the direction of a sort in a query or list view.
 *
 * @see Ordering
 * @see org.skyve.persistence.DocumentQuery#addBoundOrdering(String, SortDirection)
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum SortDirection {
	/** Sort from smallest to largest (A–Z, 0–9, earliest–latest). */
	ascending,
	/** Sort from largest to smallest (Z–A, 9–0, latest–earliest). */
	descending;

	/**
	 * Returns the opposite sort direction.
	 *
	 * @return {@link #descending} if this is {@link #ascending}; {@link #ascending} otherwise
	 */
	public SortDirection reverse() {
		SortDirection result = null;

		if (this == ascending) {
			result = descending;
		}
		else {
			result = ascending;
		}

		return result;
	}
}
