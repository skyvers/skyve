package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.view.widget.bound.Parameter;

public class FluentParameter {
	private ParameterImpl parameter = null;
	
	public FluentParameter() {
		parameter = new ParameterImpl();
	}
	
	public FluentParameter(ParameterImpl parameter) {
		this.parameter = parameter;
	}
	
	public FluentParameter from(@SuppressWarnings("hiding") Parameter parameter) {
		name(parameter.getName());
		value(parameter.getValue());
		valueBinding(parameter.getValueBinding());
		return this;
	}
	
	public FluentParameter name(String name) {
		parameter.setName(name);
		return this;
	}

	public FluentParameter value(String value) {
		parameter.setValue(value);
		return this;
	}
	
	public FluentParameter valueBinding(String valueBinding) {
		parameter.setValueBinding(valueBinding);
		return this;
	}
	
	public ParameterImpl get() {
		return parameter;
	}
}
