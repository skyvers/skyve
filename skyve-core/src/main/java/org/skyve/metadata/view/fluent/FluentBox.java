package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.ShrinkWrap;

/**
 * Defines fluent APIs shared by box-style containers.
 *
 * @param <T>
 *            the fluent type returned for method chaining
 */
interface FluentBox<T extends FluentBox<T>> extends FluentRelativeSize<T> {
	/**
	 * Configures shrink-wrap behaviour for this box.
	 *
	 * @param shrinkWrap
	 *            the shrink-wrap mode
	 * @return the fluent builder
	 */
	T shrinkWrap(ShrinkWrap shrinkWrap);

	/**
	 * Sets the container padding in pixels.
	 *
	 * @param pixelPadding
	 *            the padding applied around the box content
	 * @return the fluent builder
	 */
	T pixelPadding(int pixelPadding);

	/**
	 * Sets the spacing between members in pixels.
	 *
	 * @param pixelMemberPadding
	 *            the member gap in pixels
	 * @return the fluent builder
	 */
	T pixelMemberPadding(int pixelMemberPadding);
}
