package org.skyve.metadata.model.document;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Describes the type of domain values possible on a field
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public enum DomainType {
	/**
	 * Constant bunch of values (able to be cached for each customer, or fat client)
	 */
	constant,
	
	/**
	 * Values change but fixed for a fat client conversation or thin client render response.
	 */
	variant, 
	
	/**
	 * Values change based on the bean, ie need to calculate the values for each bean in a collection.
	 */
	dynamic
}
