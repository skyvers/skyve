package org.skyve.impl.metadata.view;

/**
 * Mixin interface for view widgets that declare both width and height constraints.
 *
 * <p>Combines {@link ConstrainableHeight} (min/max height) with
 * {@link RelativeSize} (width variants) into a single convenience interface
 * implemented by resizable container widgets.
 */
public interface ConstrainableSize extends ConstrainableHeight {
	public Integer getMinPixelWidth();
	public void setMinPixelWidth(Integer minPixelWidth);
	public Integer getMaxPixelWidth();
	public void setMaxPixelWidth(Integer maxPixelWidth);
}
