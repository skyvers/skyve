package org.skyve.impl.metadata.view.container;

import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

public interface Box extends ShrinkWrapper, Invisible, DecoratedMetaData {
	public Integer getPixelPadding();
	public void setPixelPadding(Integer pixelPadding);
	public Integer getPixelMemberPadding();
	public void setPixelMemberPadding(Integer pixelMemberPadding);
	
	public Collapsible getCollapsible();
}
