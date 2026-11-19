package org.skyve.metadata.view.fluent;

import jakarta.annotation.Nonnull;

/**
 * Defines fluent APIs for responsive grid-width breakpoints.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentResponsiveWidth<T extends FluentResponsiveWidth<T>> extends FluentRelativeWidth<T> {
	/**
	 * Sets the small breakpoint width.
	 *
	 * @param sm
	 *            the small breakpoint width value
	 * @return the fluent builder
	 */
	@Nonnull T sm(int sm);

	/**
	 * Sets the medium breakpoint width.
	 *
	 * @param md
	 *            the medium breakpoint width value
	 * @return the fluent builder
	 */
	@Nonnull T md(int md);

	/**
	 * Sets the large breakpoint width.
	 *
	 * @param lg
	 *            the large breakpoint width value
	 * @return the fluent builder
	 */
	@Nonnull T lg(int lg);

	/**
	 * Sets the extra-large breakpoint width.
	 *
	 * @param xl
	 *            the extra-large breakpoint width value
	 * @return the fluent builder
	 */
	@Nonnull T xl(int xl);
}
