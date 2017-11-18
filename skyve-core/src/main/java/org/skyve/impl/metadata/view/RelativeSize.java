package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableSize;

public interface RelativeSize extends AbsoluteSize, ConstrainableSize {
	public Integer getPercentageWidth();
	public void setPercentageWidth(Integer percentageWidth);
	public Integer getResponsiveWidth();
	public void setResponsiveWidth(Integer responsiveWidth);
	public Integer getPercentageHeight();
	public void setPercentageHeight(Integer percentageHeight);
}
