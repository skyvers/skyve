package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ParameterizableAction;

/**
 * Adds parameter-list behaviour to fluent action builders.
 *
 * @param <T>
 *            the concrete fluent action type
 */
public abstract class FluentParameterizableAction<T extends FluentParameterizableAction<T>> extends FluentPositionableAction<T> {
	/**
	 * Creates a parameterizable action fluent base.
	 */
	protected FluentParameterizableAction() {
		// nothing to see here
	}
	
	/**
	 * Copies parameterizable action metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(ParameterizableAction action) {
		super.from(action);
		action.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));
		return (T) this;
	}
	
	/**
	 * Appends a parameter definition to the wrapped action.
	 *
	 * @param parameter
	 *            the parameter to append
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addParameter(FluentParameter parameter) {
		get().getParameters().add(parameter.get());
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public abstract ParameterizableAction get();
}
