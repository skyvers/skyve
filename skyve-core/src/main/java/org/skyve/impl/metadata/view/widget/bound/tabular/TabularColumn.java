package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

public interface TabularColumn extends SerializableMetaData {
	public String getTitle();
	public default String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	public void setTitle(String title);
	public HorizontalAlignment getAlignment();
	public void setAlignment(HorizontalAlignment alignment);
}
