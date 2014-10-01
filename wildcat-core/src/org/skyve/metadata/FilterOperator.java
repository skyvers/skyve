package org.skyve.metadata;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

/**
 * 
 */
@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
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
