package org.skyve.wildcat.metadata.view;

public interface RelativeSize extends AbsoluteSize, ConstrainableSize {
	public Integer getPercentageWidth();
	public void setPercentageWidth(Integer percentageWidth);
	public Integer getPercentageHeight();
	public void setPercentageHeight(Integer percentageHeight);
}
