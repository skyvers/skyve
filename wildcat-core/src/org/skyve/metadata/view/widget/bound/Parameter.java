package org.skyve.metadata.view.widget.bound;

import org.skyve.metadata.NamedMetaData;

/**
 * 
 */
public interface Parameter extends Bound, NamedMetaData {
	/**
	 * 
	 * @return
	 */
	public String getClientId();
	
	/**
	 * 
	 * @return
	 */
	public String getValue();
}
