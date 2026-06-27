package org.skyve.metadata.view.fluent;

/**
 * Defines fluent APIs that set absolute pixel dimensions on a widget.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentAbsoluteSize<T extends FluentAbsoluteSize<T>> extends FluentAbsoluteWidth<T> {
	/**
	 * Sets the widget height in pixels.
	 *
	 * @param height
	 *            the absolute height in pixels
	 * @return the fluent builder
	 */
	T pixelHeight(int height);
}
