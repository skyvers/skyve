package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

/**
 * Provides fluent operations shared by input widgets with change actions.
 *
 * @param <T> the concrete fluent builder type
 */
public abstract class FluentChangeableInputWidget<T extends FluentChangeableInputWidget<T>>  extends FluentFocusableInputWidget<T> {
	/**
	 * Creates a new fluent changeable-input-widget builder.
	 */
	protected FluentChangeableInputWidget() {
		// nothing to see
	}
	
	/**
	 * Copies shared change action bindings from the source widget metadata.
	 */
	@SuppressWarnings("unchecked")
	protected T from(ChangeableInputWidget input) {
		super.from(input);
		input.getChangedActions().forEach(a -> addChangeAction(FluentEventAction.from(a)));
		return (T) this;
	}

	/**
	 * Appends a change action to the wrapped widget metadata.
	 */
	@SuppressWarnings("unchecked")
	public T addChangeAction(FluentEventAction action) {
		get().getChangedActions().add(action.get());
		return (T) this;
	}

	/**
	 * Returns the wrapped {@link ChangeableInputWidget} metadata instance.
	 *
	 * @return the mutable changeable-input-widget metadata being configured
	 */
	@Override
	public abstract ChangeableInputWidget get();
}
