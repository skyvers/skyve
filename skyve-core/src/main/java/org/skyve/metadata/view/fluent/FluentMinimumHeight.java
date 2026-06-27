package org.skyve.metadata.view.fluent;

/**
 * Defines fluent APIs for minimum-height constraints.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentMinimumHeight<T extends FluentMinimumHeight<T>> {
	/**
	 * Sets the minimum widget height in pixels.
	 *
	 * @param minPixelHeight
	 *            the minimum allowed height in pixels
	 * @return the fluent builder
	 */
	T minPixelHeight(int minPixelHeight);
}
