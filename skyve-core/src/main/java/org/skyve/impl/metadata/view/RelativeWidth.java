package org.skyve.impl.metadata.view;

public interface RelativeWidth extends AbsoluteWidth {
	public Integer getPercentageWidth();
	public void setPercentageWidth(Integer percentageWidth);
	public Integer getResponsiveWidth();
	public void setResponsiveWidth(Integer responsiveWidth);
}
