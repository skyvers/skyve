package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.view.ConstrainableHeight;

public interface ConstrainableSize extends ConstrainableHeight {
	public Integer getMinPixelWidth();
	public void setMinPixelWidth(Integer minPixelWidth);
	public Integer getMaxPixelWidth();
	public void setMaxPixelWidth(Integer maxPixelWidth);
}
