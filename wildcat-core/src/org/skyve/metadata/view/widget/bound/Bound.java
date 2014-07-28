package org.skyve.metadata.view.widget.bound;

import org.skyve.metadata.MetaData;

/**
 * 
 */
public interface Bound extends MetaData {
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
