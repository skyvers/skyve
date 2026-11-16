package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ParameterizableAction;

import jakarta.annotation.Nonnull;

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
	protected @Nonnull T from(@Nonnull ParameterizableAction action) {
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
	public @Nonnull T addParameter(@Nonnull FluentParameter parameter) {
		get().getParameters().add(parameter.get());
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public abstract @Nonnull ParameterizableAction get();
}
