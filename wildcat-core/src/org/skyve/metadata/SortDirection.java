package org.skyve.metadata;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

/**
 * 
 */
@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
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
