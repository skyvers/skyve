package org.skyve.metadata.view.fluent;

/**
 * Defines fluent APIs that set an absolute pixel width on a widget.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentAbsoluteWidth<T extends FluentAbsoluteWidth<T>> {
	/**
	 * Sets the widget width in pixels.
	 *
	 * @param width
	 *            the absolute width in pixels
	 * @return the fluent builder
	 */
	T pixelWidth(int width);
}
