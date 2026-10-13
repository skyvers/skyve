package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.FocusableInputWidget;

import jakarta.annotation.Nonnull;

/**
 * Provides fluent operations shared by focus-aware input widget metadata builders.
 *
 * @param <T> the concrete fluent builder type
 */
public abstract class FluentFocusableInputWidget<T extends FluentFocusableInputWidget<T>>  extends FluentInputWidget<T> {
	/**
	 * Creates a new fluent focusable-input-widget builder.
	 */
	protected FluentFocusableInputWidget() {
		// nothing to see
	}
	
	/**
	 * Copies shared focus and blur action bindings from the source widget metadata.
	 */
	@SuppressWarnings("unchecked")
	protected @Nonnull T from(@Nonnull FocusableInputWidget input) {
		super.from(input);
		input.getFocusActions().forEach(a -> addFocusAction(FluentEventAction.from(a)));
		input.getBlurActions().forEach(a -> addBlurAction(FluentEventAction.from(a)));
		return (T) this;
	}

	/**
	 * Appends a focus action to the wrapped widget metadata.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T addFocusAction(@Nonnull FluentEventAction action) {
		get().getFocusActions().add(action.get());
		return (T) this;
	}

	/**
	 * Appends a blur action to the wrapped widget metadata.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T addBlurAction(@Nonnull FluentEventAction action) {
		get().getBlurActions().add(action.get());
		return (T) this;
	}

	/**
	 * Returns the wrapped {@link FocusableInputWidget} metadata instance.
	 *
	 * @return the mutable focusable-input-widget metadata being configured
	 */
	@Override
	public abstract @Nonnull FocusableInputWidget get();
}
