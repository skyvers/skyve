package org.skyve.metadata.view.widget.bound;

import org.skyve.metadata.NamedMetaData;

/**
 * 
 */
public interface Parameter extends NamedMetaData {
	public String getValue();
	public String getValueBinding();
}
