package org.skyve.wildcat.metadata.view.widget.bound.tabular;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.metadata.view.HorizontalAlignment;

public interface TabularColumn extends MetaData {
	public String getTitle();
	public void setTitle(String title);
	public HorizontalAlignment getAlignment();
	public void setAlignment(HorizontalAlignment alignment);
}
