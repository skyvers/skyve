package org.skyve.metadata.view.widget;

import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.MetaData;

public interface FilterParameter extends MetaData {
	public String getFilterBinding();
	public FilterOperator getOperator();
	public String getValue();
	public String getValueBinding();
}
