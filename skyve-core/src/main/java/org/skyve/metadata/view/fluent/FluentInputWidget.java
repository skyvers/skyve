package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

/**
 * Provides fluent operations shared by input widget metadata builders.
 *
 * @param <T> the concrete fluent builder type
 */
public abstract class FluentInputWidget<T extends FluentInputWidget<T>>  extends FluentBoundWidget<T> {
	/**
	 * Creates a new fluent input-widget builder.
	 */
	protected FluentInputWidget() {
		// nothing to see
	}
	
	/**
	 * Copies shared input-widget state from the source metadata instance.
	 */
	@SuppressWarnings("unchecked")
	protected T from(InputWidget input) {
		super.from(input);
		disabledConditionName(input.getDisabledConditionName());
		invisibleConditionName(input.getInvisibleConditionName());
		return (T) this;
	}

	/**
	 * Sets the condition that disables this widget when it evaluates to true.
	 */
	@SuppressWarnings("unchecked")
	public T disabledConditionName(String disabledConditionName) {
		get().setDisabledConditionName(disabledConditionName);
		return (T) this;
	}

	/**
	 * Sets the condition that hides this widget when it evaluates to true.
	 */
	@SuppressWarnings("unchecked")
	public T invisibleConditionName(String invisibleConditionName) {
		get().setInvisibleConditionName(invisibleConditionName);
		return (T) this;
	}

	/**
	 * Returns the wrapped {@link InputWidget} metadata instance.
	 *
	 * @return the mutable input-widget metadata being configured
	 */
	@Override
	public abstract InputWidget get();
}
