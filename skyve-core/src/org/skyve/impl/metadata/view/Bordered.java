package org.skyve.impl.metadata.view;

import org.skyve.metadata.MetaData;

public interface Bordered extends MetaData {
	public Boolean getBorder();
	public void setBorder(Boolean border);
	public String getBorderTitle();
	public void setBorderTitle(String borderTitle);
}
