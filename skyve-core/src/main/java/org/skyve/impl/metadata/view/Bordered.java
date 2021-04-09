package org.skyve.impl.metadata.view;

import org.skyve.metadata.MetaData;
import org.skyve.util.Util;

public interface Bordered extends MetaData {
	public Boolean getBorder();
	public void setBorder(Boolean border);
	public String getBorderTitle();
	public default String getLocalisedBorderTitle() {
		return Util.i18n(getBorderTitle());
	}
	public void setBorderTitle(String borderTitle);
}
