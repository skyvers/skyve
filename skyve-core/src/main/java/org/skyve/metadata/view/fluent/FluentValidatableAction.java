package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ValidatableAction;

/**
 * Adds client-validation controls to fluent action builders.
 *
 * @param <T>
 *            the concrete fluent action type
 */
public abstract class FluentValidatableAction<T extends FluentValidatableAction<T>> extends FluentPositionableAction<T> {
	/**
	 * Creates a validatable action fluent base.
	 */
	protected FluentValidatableAction() {
		// nothing to see here
	}
	
	/**
	 * Copies validation-related metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(ValidatableAction action) {
		super.from(action);
		clientValidation(! Boolean.FALSE.equals(action.getClientValidation()));
		return (T) this;
	}
	
	/**
	 * Sets whether client-side validation should run before execution.
	 *
	 * @param clientValidation
	 *            whether to require client validation
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T clientValidation(boolean clientValidation) {
		get().setClientValidation(clientValidation ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public abstract ValidatableAction get();
}
