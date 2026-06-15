package org.skyve.metadata;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
/**
 * Comparison operators available in Skyve query filters and list view column filters.
 *
 * <p>These operators correspond to the {@code addXxx} methods on
 * {@link org.skyve.persistence.DocumentFilter}. The XML representation uses these
 * enum constants directly in query-column filter definitions.
 *
 * <p>The {@code nullOr*} variants evaluate as true when the field value is SQL NULL
 * as well as when the stated condition holds; see
 * {@link org.skyve.persistence.DocumentFilter} for the semantics of each variant.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum FilterOperator {
	/**
	 * 
	 */
	equal,
	
	/**
	 * 
	 */
	greater,
	
	/**
	 * 
	 */
	less,
	
	/**
	 * 
	 */
	greaterEqual,
	
	/**
	 * 
	 */
	lessEqual,
	
	/**
	 * 
	 */
	notEqual,
	
	/**
	 * 
	 */
	like, 
	
	/**
	 * 
	 */
	notLike, 
	
	/**
	 * 
	 */
	notNull,
	
	/**
	 * 
	 */
	isNull,
	
	/**
	 * 
	 */
	nullOrEqual,
	
	/**
	 * 
	 */
	nullOrGreater, 
	
	/**
	 * 
	 */
	nullOrLess,
	
	/**
	 * 
	 */
	nullOrGreaterEqual, 
	
	/**
	 * 
	 */
	nullOrLessEqual, 
	
	/**
	 * 
	 */
	nullOrNotEqual,
	
	/**
	 * 
	 */
	nullOrLike, 
	
	/**
	 * 
	 */
	nullOrNotLike;
}
