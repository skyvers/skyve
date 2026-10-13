package org.skyve.metadata.view.fluent;

import jakarta.annotation.Nonnull;

/**
 * Defines fluent APIs for relative width configuration.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentRelativeWidth<T extends FluentRelativeWidth<T>> extends FluentAbsoluteWidth<T> {
	/**
	 * Sets the width as a percentage of the available container width.
	 *
	 * @param width
	 *            the percentage width
	 * @return the fluent builder
	 */
	@Nonnull T percentageWidth(int width);

	/**
	 * Sets the responsive width breakpoint value.
	 *
	 * @param width
	 *            the responsive width value
	 * @return the fluent builder
	 */
	@Nonnull T responsiveWidth(int width);
}
