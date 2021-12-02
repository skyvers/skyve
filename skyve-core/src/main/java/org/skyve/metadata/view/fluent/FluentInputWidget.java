package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

abstract class FluentInputWidget<T extends FluentInputWidget<T>>  extends FluentBoundWidget<T> {
	protected FluentInputWidget() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(InputWidget input) {
		disabledConditionName(input.getDisabledConditionName());
		invisibleConditionName(input.getInvisibleConditionName());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T disabledConditionName(String disabledConditionName) {
		get().setDisabledConditionName(disabledConditionName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T invisibleConditionName(String invisibleConditionName) {
		get().setInvisibleConditionName(invisibleConditionName);
		return (T) this;
	}

	@Override
	public abstract InputWidget get();
}
