package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ClassAction;

/**
 * Adds class-targeting behaviour to fluent action builders.
 *
 * @param <T>
 *            the concrete fluent action type
 */
public abstract class FluentClassAction<T extends FluentClassAction<T>> extends FluentValidatableAction<T> {
	/**
	 * Creates a class-action fluent base.
	 */
	protected FluentClassAction() {
		// nothing to see here
	}
	
	/**
	 * Copies class-action metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(ClassAction action) {
		super.from(action);
		className(action.getClassName());
		return (T) this;
	}
	
	/**
	 * Sets the backing class name resolved for this action.
	 *
	 * @param className
	 *            the class name or bean binding used by the action
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T className(String className) {
		get().setClassName(className);
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public abstract ClassAction get();
}
