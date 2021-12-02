package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.FocusableInputWidget;

abstract class FluentFocusableInputWidget<T extends FluentFocusableInputWidget<T>>  extends FluentInputWidget<T> {
	protected FluentFocusableInputWidget() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(FocusableInputWidget input) {
		super.from(input);
		input.getFocusActions().forEach(a -> addFocusAction(FluentEventAction.from(a)));
		input.getBlurActions().forEach(a -> addBlurAction(FluentEventAction.from(a)));
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addFocusAction(FluentEventAction action) {
		get().getFocusActions().add(action.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addBlurAction(FluentEventAction action) {
		get().getFocusActions().add(action.get());
		return (T) this;
	}

	@Override
	public abstract FocusableInputWidget get();
}
