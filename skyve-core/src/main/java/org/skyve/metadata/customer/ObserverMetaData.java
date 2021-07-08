package org.skyve.metadata.customer;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.Observer;

/**
 * 
 */
public interface ObserverMetaData extends MetaData {
	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	public Observer getObserver();
}
