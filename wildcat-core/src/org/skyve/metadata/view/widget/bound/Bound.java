package org.skyve.metadata.view.widget.bound;

import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.metadata.MetaData;

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
