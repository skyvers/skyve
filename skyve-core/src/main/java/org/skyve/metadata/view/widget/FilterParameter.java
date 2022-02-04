package org.skyve.metadata.view.widget;

import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SerializableMetaData;

public interface FilterParameter extends SerializableMetaData {
	public String getFilterBinding();
	public FilterOperator getOperator();
	public String getValue();
	public String getValueBinding();
}
