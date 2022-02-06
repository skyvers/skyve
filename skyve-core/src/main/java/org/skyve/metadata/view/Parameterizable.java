package org.skyve.metadata.view;

import java.util.List;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * 
 */
public interface Parameterizable extends SerializableMetaData {
	/**
	 * 
	 * @return
	 */
	public List<Parameter> getParameters();
}
