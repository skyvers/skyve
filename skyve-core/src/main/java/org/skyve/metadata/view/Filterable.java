package org.skyve.metadata.view;

import java.util.List;

import org.skyve.metadata.view.widget.bound.FilterParameter;

/**
 * 
 */
public interface Filterable extends Parameterizable {
	/**
	 * 
	 * @return
	 */
	public List<FilterParameter> getFilterParameters();
}
