package org.skyve.wildcat.metadata.view;

public interface ConstrainableSize extends ConstrainableHeight {
	public Integer getMinPixelWidth();
	public void setMinPixelWidth(Integer minPixelWidth);
	public Integer getMaxPixelWidth();
	public void setMaxPixelWidth(Integer maxPixelWidth);
}
