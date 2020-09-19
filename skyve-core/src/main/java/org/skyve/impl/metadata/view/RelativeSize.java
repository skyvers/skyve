package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableSize;

public interface RelativeSize extends AbsoluteSize, ConstrainableSize {
	public Integer getPercentageWidth();
	public void setPercentageWidth(Integer percentageWidth);
	public Integer getResponsiveWidth();
	public void setResponsiveWidth(Integer responsiveWidth);
	public Integer getSm();
	public void setSm(Integer sm);
	public Integer getMd();
	public void setMd(Integer md);
	public Integer getLg();
	public void setLg(Integer lg);
	public Integer getXl();
	public void setXl(Integer xl);
	public Integer getPercentageHeight();
	public void setPercentageHeight(Integer percentageHeight);
}
