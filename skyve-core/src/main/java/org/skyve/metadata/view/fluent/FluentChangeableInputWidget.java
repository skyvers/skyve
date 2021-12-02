package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

abstract class FluentChangeableInputWidget<T extends FluentChangeableInputWidget<T>>  extends FluentFocusableInputWidget<T> {
	protected FluentChangeableInputWidget() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ChangeableInputWidget input) {
		super.from(input);
		input.getChangedActions().forEach(a -> addChangeAction(FluentEventAction.from(a)));
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addChangeAction(FluentEventAction action) {
		get().getChangedActions().add(action.get());
		return (T) this;
	}

	@Override
	public abstract ChangeableInputWidget get();
}
