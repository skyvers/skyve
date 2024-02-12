package org.skyve.metadata;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum SortDirection {
	ascending, descending;

	/**
	 * 
	 * @return
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
