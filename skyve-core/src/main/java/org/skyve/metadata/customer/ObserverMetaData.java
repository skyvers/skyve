package org.skyve.metadata.customer;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.controller.Observer;

/**
 * 
 */
public interface ObserverMetaData extends SerializableMetaData {
	/**
	 * 
	 * @return
	 */
	public String getClassName();
	
	public Observer getObserver();
}
