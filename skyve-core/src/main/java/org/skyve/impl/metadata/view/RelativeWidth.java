package org.skyve.impl.metadata.view;

/**
 * Mixin interface for view widgets that declare a relative (percentage or column)
 * width.
 *
 * <p>Extends {@link AbsoluteWidth} with a percentage-based or responsive-column
 * width attribute, overriding the absolute pixel width when both are present.
 */
public interface RelativeWidth extends AbsoluteWidth {
	public Integer getPercentageWidth();
	public void setPercentageWidth(Integer percentageWidth);
	public Integer getResponsiveWidth();
	public void setResponsiveWidth(Integer responsiveWidth);
}
