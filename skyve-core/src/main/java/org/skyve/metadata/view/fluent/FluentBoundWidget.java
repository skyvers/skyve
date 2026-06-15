package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.AbstractBound;

/**
 * Provides fluent operations shared by bound widget metadata builders.
 *
 * @param <T> the concrete fluent builder type
 */
public abstract class FluentBoundWidget<T extends FluentBoundWidget<T>> extends FluentWidget {
	/**
	 * Creates a new fluent bound-widget builder.
	 */
	protected FluentBoundWidget() {
		// nothing to see
	}
	
	/**
	 * Copies bound-widget state from the source metadata instance.
	 *
	 * @param bound
	 *            the source metadata to copy
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T from(AbstractBound bound) {
		binding(bound.getBinding());
		return (T) this;
	}
	
	/**
	 * Sets the binding expression resolved for the wrapped widget.
	 *
	 * @param binding
	 *            the binding expression
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T binding(String binding) {
		get().setBinding(binding);
		return (T) this;
	}

	/**
	 * Returns the wrapped bound-widget metadata instance being configured.
	 */
	@Override
	public abstract AbstractBound get();
}
