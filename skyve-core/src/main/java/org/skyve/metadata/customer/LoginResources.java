package org.skyve.metadata.customer;

import org.skyve.metadata.MetaData;

/**
 * 
 */
public interface LoginResources extends MetaData {
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
