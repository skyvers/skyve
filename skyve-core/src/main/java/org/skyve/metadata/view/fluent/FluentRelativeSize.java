package org.skyve.metadata.view.fluent;

import jakarta.annotation.Nonnull;

/**
 * Defines fluent APIs for widgets that support both relative and absolute dimensions.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentRelativeSize<T extends FluentRelativeSize<T>> extends FluentAbsoluteSize<T>, FluentResponsiveWidth<T>, FluentConstrainableSize<T> {
	/**
	 * Sets the height as a percentage of available container height.
	 *
	 * @param percentageHeight
	 *            the percentage height value
	 * @return the fluent builder
	 */
	@Nonnull T percentageHeight(int percentageHeight);
}
