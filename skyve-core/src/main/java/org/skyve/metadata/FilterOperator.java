package org.skyve.metadata;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * 
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
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
