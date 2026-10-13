package org.skyve.impl.metadata.view;

import jakarta.annotation.Nullable;

/**
 * Mixin interface for view widgets that declare a responsive grid column width.
 *
 * <p>Implemented by JAXB widget descriptors that expose a
 * {@code responsiveWidth} attribute for CSS grid-column sizing.
 * Extends {@link RelativeWidth}.
 */
public interface ResponsiveWidth extends RelativeWidth {
	public @Nullable Integer getSm();
	public void setSm(@Nullable Integer sm);
	public @Nullable Integer getMd();
	public void setMd(@Nullable Integer md);
	public @Nullable Integer getLg();
	public void setLg(@Nullable Integer lg);
	public @Nullable Integer getXl();
	public void setXl(@Nullable Integer xl);
}
