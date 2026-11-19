package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare both a minimum and maximum pixel
 * height constraint.
 *
 * <p>Extends {@link MinimumHeight} with a {@code maxPixelHeight} attribute.
 */
public interface ConstrainableHeight extends MinimumHeight {
	public @Nullable Integer getMaxPixelHeight();
	public void setMaxPixelHeight(@Nullable Integer maxPixelHeight);
}
