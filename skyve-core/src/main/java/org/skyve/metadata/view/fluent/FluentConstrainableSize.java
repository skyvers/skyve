package org.skyve.metadata.view.fluent;

/**
 * Defines fluent APIs for minimum and maximum size constraints.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentConstrainableSize<T extends FluentConstrainableSize<T>> extends FluentConstrainableHeight<T> {
	/**
	 * Sets the minimum widget width in pixels.
	 *
	 * @param minPixelWidth
	 *            the minimum allowed width in pixels
	 * @return the fluent builder
	 */
	T minPixelWidth(int minPixelWidth);

	/**
	 * Sets the maximum widget width in pixels.
	 *
	 * @param maxPixelWidth
	 *            the maximum allowed width in pixels
	 * @return the fluent builder
	 */
	T maxPixelWidth(int maxPixelWidth);
}
