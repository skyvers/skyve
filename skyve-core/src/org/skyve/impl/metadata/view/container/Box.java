package org.skyve.impl.metadata.view.container;

import org.skyve.impl.metadata.view.ShrinkWrapper;
import org.skyve.metadata.DecoratedMetaData;

public interface Box extends ShrinkWrapper, DecoratedMetaData {
	public Integer getPixelPadding();
	public void setPixelPadding(Integer pixelPadding);
	public Integer getPixelMemberPadding();
	public void setPixelMemberPadding(Integer pixelMemberPadding);
}
