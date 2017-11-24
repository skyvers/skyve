package org.skyve.metadata;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

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
