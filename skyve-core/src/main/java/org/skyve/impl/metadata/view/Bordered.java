package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

public interface Bordered extends SerializableMetaData {
	public Boolean getBorder();
	public void setBorder(Boolean border);
	public String getBorderTitle();
	public default String getLocalisedBorderTitle() {
		return Util.i18n(getBorderTitle());
	}
	public void setBorderTitle(String borderTitle);
}
