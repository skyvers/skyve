package org.skyve.impl.metadata.view;

public interface RelativeSize extends AbsoluteSize, ResponsiveWidth, ConstrainableSize {
	public Integer getPercentageHeight();
	public void setPercentageHeight(Integer percentageHeight);
}
