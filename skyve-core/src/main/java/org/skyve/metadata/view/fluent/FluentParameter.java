package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.metadata.view.widget.bound.Parameter;

/**
 * Builds query {@link Parameter} metadata used by list/query widgets.
 */
public class FluentParameter {
	private ParameterImpl parameter = null;
	
	/**
	 * Creates a builder backed by a new {@link ParameterImpl}.
	 */
	public FluentParameter() {
		parameter = new ParameterImpl();
	}
	
	/**
	 * Creates a builder backed by the supplied {@link ParameterImpl}.
	 *
	 * @param parameter
	 *            the metadata instance to mutate
	 */
	public FluentParameter(ParameterImpl parameter) {
		this.parameter = parameter;
	}
	
	/**
	 * Copies parameter state from runtime metadata.
	 *
	 * @param parameter
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentParameter from(@SuppressWarnings("hiding") Parameter parameter) {
		name(parameter.getName());
		value(parameter.getValue());
		valueBinding(parameter.getValueBinding());
		return this;
	}
	
	/**
	 * Sets the parameter name expected by the target query or action.
	 *
	 * @param name
	 *            the parameter name
	 * @return this builder
	 */
	public FluentParameter name(String name) {
		parameter.setName(name);
		return this;
	}

	/**
	 * Sets the literal parameter value.
	 *
	 * @param value
	 *            the literal parameter value
	 * @return this builder
	 */
	public FluentParameter value(String value) {
		parameter.setValue(value);
		return this;
	}
	
	/**
	 * Sets the binding that supplies the parameter value.
	 *
	 * @param valueBinding
	 *            the value-binding expression
	 * @return this builder
	 */
	public FluentParameter valueBinding(String valueBinding) {
		parameter.setValueBinding(valueBinding);
		return this;
	}
	
	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped parameter metadata
	 */
	public ParameterImpl get() {
		return parameter;
	}
}
