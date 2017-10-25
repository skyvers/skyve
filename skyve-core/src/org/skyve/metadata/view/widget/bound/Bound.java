package org.skyve.metadata.view.widget.bound;

import org.skyve.impl.metadata.view.event.EventSource;

/**
 * 
 */
public interface Bound extends EventSource {
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
