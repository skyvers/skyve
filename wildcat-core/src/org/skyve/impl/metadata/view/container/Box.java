package org.skyve.impl.metadata.view.container;

import org.skyve.impl.metadata.view.ShrinkWrapper;

public interface Box extends ShrinkWrapper {
	public Integer getPixelPadding();
	public void setPixelPadding(Integer pixelPadding);
	public Integer getPixelMemberPadding();
	public void setPixelMemberPadding(Integer pixelMemberPadding);
}
