package org.skyve.impl.metadata.view;

/**
 * Mixin interface for view widgets that declare a responsive grid column width.
 *
 * <p>Implemented by JAXB widget descriptors that expose a
 * {@code responsiveWidth} attribute for CSS grid-column sizing.
 * Extends {@link RelativeWidth}.
 */
public interface ResponsiveWidth extends RelativeWidth {
	public Integer getSm();
	public void setSm(Integer sm);
	public Integer getMd();
	public void setMd(Integer md);
	public Integer getLg();
	public void setLg(Integer lg);
	public Integer getXl();
	public void setXl(Integer xl);
}
