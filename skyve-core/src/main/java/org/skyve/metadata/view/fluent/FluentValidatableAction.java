package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ValidatableAction;

abstract class FluentValidatableAction<T extends FluentValidatableAction<T>> extends FluentPositionableAction<T> {
	protected FluentValidatableAction() {
		// nothing to see here
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ValidatableAction action) {
		super.from(action);
		clientValidation(! Boolean.FALSE.equals(action.getClientValidation()));
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T clientValidation(boolean clientValidation) {
		get().setClientValidation(clientValidation ? Boolean.TRUE : Boolean.FALSE);
		return (T) this;
	}

	@Override
	public abstract ValidatableAction get();
}
