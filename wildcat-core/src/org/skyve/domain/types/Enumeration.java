package org.skyve.domain.types;

import org.skyve.metadata.MetaData;

/**
 * 
 */
public interface Enumeration extends MetaData {
	/**
	 * 
	 */
	public static final String TO_CODE_METHOD_NAME = "toCode";
	
	/**
	 * 
	 */
	public static final String FROM_CODE_METHOD_NAME = "fromCode";
	
	/**
	 * 
	 */
	public static final String FROM_DESCRIPTION_METHOD_NAME = "fromDescription";
	
	/**
	 * 
	 * @return
	 */
	public String toCode();
	
	/**
	 * 
	 * @return
	 */
	public String toDescription();
}
