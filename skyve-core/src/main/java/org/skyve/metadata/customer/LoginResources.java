package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;

/**
 * 
 */
public interface LoginResources extends SerializableMetaData {
	/**
	 * 
	 * @return
	 */
	public String getLoginPageURL();
	
	/**
	 * 
	 * @return
	 */
	public String getLoggedOutPageURL();
	
	/**
	 * 
	 * @return
	 */
	public String getSmartClientJavascriptURL();
}
