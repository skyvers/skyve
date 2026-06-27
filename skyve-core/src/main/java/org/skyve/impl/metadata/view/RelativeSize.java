package org.skyve.impl.metadata.view;

/**
 * Mixin interface for view widgets that support the full set of size attributes:
 * absolute pixel size, responsive column width, and height constraints.
 *
 * <p>Combines {@link AbsoluteSize}, {@link ResponsiveWidth}, and
 * {@link ConstrainableSize} into a single convenience interface implemented by
 * most content container widgets.
 */
public interface RelativeSize extends AbsoluteSize, ResponsiveWidth, ConstrainableSize {
	public Integer getPercentageHeight();
	public void setPercentageHeight(Integer percentageHeight);
}
