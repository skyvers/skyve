package org.skyve.impl.metadata.view.widget.bound.tabular;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.MetaData;

public interface TabularColumn extends MetaData {
	public String getTitle();
	public void setTitle(String title);
	public HorizontalAlignment getAlignment();
	public void setAlignment(HorizontalAlignment alignment);
}
