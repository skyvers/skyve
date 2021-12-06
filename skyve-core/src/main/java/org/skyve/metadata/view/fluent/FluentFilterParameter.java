package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.view.widget.FilterParameter;

public class FluentFilterParameter {
	private FilterParameterImpl filterParameter = null;

	public FluentFilterParameter() {
		filterParameter = new FilterParameterImpl();
	}

	public FluentFilterParameter(FilterParameterImpl filterParameter) {
		this.filterParameter = filterParameter;
	}

	public FluentFilterParameter from(@SuppressWarnings("hiding") FilterParameter filterParameter) {
		filterBinding(filterParameter.getFilterBinding());
		value(filterParameter.getValue());
		valueBinding(filterParameter.getValueBinding());
		operator(filterParameter.getOperator());
		return this;
	}

	public FluentFilterParameter filterBinding(String filterBinding) {
		filterParameter.setFilterBinding(filterBinding);
		return this;
	}

	public FluentFilterParameter value(String value) {
		filterParameter.setValue(value);
		return this;
	}

	public FluentFilterParameter valueBinding(String valueBinding) {
		filterParameter.setValueBinding(valueBinding);
		return this;
	}

	public FluentFilterParameter operator(FilterOperator operator) {
		filterParameter.setOperator(operator);
		return this;
	}

	public FilterParameterImpl get() {
		return filterParameter;
	}
}
