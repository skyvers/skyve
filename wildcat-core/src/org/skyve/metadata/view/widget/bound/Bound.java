package org.skyve.metadata.view.widget.bound;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.metadata.view.event.EventSource;

/**
 * 
 */
public interface Bound extends MetaData, EventSource {
	/**
	 * 
	 * @return
	 */
	public String getBinding();
	
	/**
	 * 
	 * @param binding
	 */
	public void setBinding(String binding);
}
