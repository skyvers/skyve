package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare a relative (percentage or column)
 * width.
 *
 * <p>Extends {@link AbsoluteWidth} with a percentage-based or responsive-column
 * width attribute, overriding the absolute pixel width when both are present.
 */
public interface RelativeWidth extends AbsoluteWidth {
	public @Nullable Integer getPercentageWidth();
	public void setPercentageWidth(@Nullable Integer percentageWidth);
	public @Nullable Integer getResponsiveWidth();
	public void setResponsiveWidth(@Nullable Integer responsiveWidth);
}
