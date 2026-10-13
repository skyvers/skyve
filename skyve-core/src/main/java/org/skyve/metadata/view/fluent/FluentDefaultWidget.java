package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;

import jakarta.annotation.Nonnull;

/**
 * Builds {@link DefaultWidget} metadata using a fluent API.
 */
public class FluentDefaultWidget extends FluentBoundWidget<FluentDefaultWidget> {
	private DefaultWidget widget = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link DefaultWidget} metadata instance.
	 */
	public FluentDefaultWidget() {
		widget = new DefaultWidget();
	}
	
	/**
	 * Creates a fluent builder backed by the supplied {@link DefaultWidget} metadata instance.
	 *
	 * @param widget the metadata instance to mutate
	 */
	public FluentDefaultWidget(DefaultWidget widget) {
		this.widget = widget;
	}
	
	/**
	 * Copies default-widget metadata into this fluent builder.
	 *
	 * @param widget
	 *            the source metadata to copy
	 * @return this builder
	 */
	public FluentDefaultWidget from(@SuppressWarnings("hiding") DefaultWidget widget) {
		super.from(widget);
		widget.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentDefaultWidget putProperty(@Nonnull String k, @Nonnull String v) {
		widget.getProperties().put(k, v);
		return this;
	}
	
	/**
	 * Returns the wrapped {@link DefaultWidget} metadata instance.
	 *
	 * @return the mutable default-widget metadata being configured
	 */
	@Override
	public DefaultWidget get() {
		return widget;
	}
}
