package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ClassAction;

abstract class FluentClassAction<T extends FluentClassAction<T>> extends FluentValidatableAction<T> {
	protected FluentClassAction() {
		// nothing to see here
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ClassAction action) {
		super.from(action);
		className(action.getClassName());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T className(String className) {
		get().setClassName(className);
		return (T) this;
	}

	@Override
	public abstract ClassAction get();
}
