package org.skyve.impl.metadata.view;

/**
 * Mixin interface for view widgets that declare both a minimum and maximum pixel
 * height constraint.
 *
 * <p>Extends {@link MinimumHeight} with a {@code maxPixelHeight} attribute.
 */
public interface ConstrainableHeight extends MinimumHeight {
	public Integer getMaxPixelHeight();
	public void setMaxPixelHeight(Integer maxPixelHeight);
}
