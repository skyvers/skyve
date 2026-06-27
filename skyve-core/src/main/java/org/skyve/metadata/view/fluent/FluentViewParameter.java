package org.skyve.metadata.view.fluent;

import org.skyve.metadata.view.View.ViewParameter;

/**
 * Builds {@link ViewParameter} metadata used by view-level parameter binding.
 */
public class FluentViewParameter {
	private ViewParameter parameter = null;
	
	/**
	 * Creates a builder backed by a new {@link ViewParameter}.
	 */
	public FluentViewParameter() {
		parameter = new ViewParameter();
	}

	/**
	 * Creates a builder backed by the supplied {@link ViewParameter}.
	 *
	 * @param parameter
	 *            the metadata instance to mutate
	 */
	public FluentViewParameter(ViewParameter parameter) {
		this.parameter = parameter;
	}
	
	/**
	 * Copies view-parameter binding state from runtime metadata.
	 *
	 * @param parameter
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentViewParameter from(@SuppressWarnings("hiding") ViewParameter parameter) {
		fromBinding(parameter.getFromBinding());
		boundTo(parameter.getBoundTo());
		return this;
	}
	
	/**
	 * Sets the source binding expression that supplies the parameter value.
	 *
	 * @param fromBinding
	 *            the binding expression evaluated to obtain the value
	 * @return this builder
	 */
	public FluentViewParameter fromBinding(String fromBinding) {
		parameter.setFromBinding(fromBinding);
		return this;
	}

	/**
	 * Sets the target parameter name or binding that receives the sourced value.
	 *
	 * @param boundTo
	 *            the target parameter destination
	 * @return this builder
	 */
	public FluentViewParameter boundTo(String boundTo) {
		parameter.setBoundTo(boundTo);
		return this;
	}
	
	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped view-parameter metadata
	 */
	public ViewParameter get() {
		return parameter;
	}
}
