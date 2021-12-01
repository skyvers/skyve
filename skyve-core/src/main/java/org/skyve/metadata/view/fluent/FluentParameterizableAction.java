package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ParameterizableAction;

abstract class FluentParameterizableAction<T extends FluentParameterizableAction<T>> extends FluentPositionableAction<T> {
	protected FluentParameterizableAction() {
		// nothing to see here
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ParameterizableAction action) {
		super.from(action);
		action.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)));
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addParameter(FluentParameter parameter) {
		get().getParameters().add(parameter.get());
		return (T) this;
	}

	@Override
	public abstract ParameterizableAction get();
}
