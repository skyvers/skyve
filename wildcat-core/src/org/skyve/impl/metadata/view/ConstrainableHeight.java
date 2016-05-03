package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.view.MinimumHeight;

public interface ConstrainableHeight extends MinimumHeight {
	public Integer getMaxPixelHeight();
	public void setMaxPixelHeight(Integer maxPixelHeight);
}
