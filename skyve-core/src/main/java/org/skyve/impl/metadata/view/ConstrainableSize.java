package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare both width and height constraints.
 *
 * <p>Combines {@link ConstrainableHeight} (min/max height) with
 * {@link RelativeSize} (width variants) into a single convenience interface
 * implemented by resizable container widgets.
 */
public interface ConstrainableSize extends ConstrainableHeight {
	public @Nullable Integer getMinPixelWidth();
	public void setMinPixelWidth(@Nullable Integer minPixelWidth);
	public @Nullable Integer getMaxPixelWidth();
	public void setMaxPixelWidth(@Nullable Integer maxPixelWidth);
}
