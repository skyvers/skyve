package org.skyve.metadata.view;

import java.util.List;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * 
 */
public interface Parameterizable extends MetaData {
	/**
	 * 
	 * @return
	 */
	public List<Parameter> getParameters();
}
