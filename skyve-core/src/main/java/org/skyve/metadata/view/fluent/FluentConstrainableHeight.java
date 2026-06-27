package org.skyve.metadata.view.fluent;

/**
 * Defines fluent APIs for bounded height constraints.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentConstrainableHeight<T extends FluentConstrainableHeight<T>> extends FluentMinimumHeight<T> {
	/**
	 * Sets the maximum widget height in pixels.
	 *
	 * @param maxPixelHeight
	 *            the maximum allowed height in pixels
	 * @return the fluent builder
	 */
	T maxPixelHeight(int maxPixelHeight);
}
