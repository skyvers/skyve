package org.skyve.impl.metadata.view;

import org.skyve.metadata.SerializableMetaData;

public interface MinimumHeight extends SerializableMetaData {
	public Integer getMinPixelHeight();
	public void setMinPixelHeight(Integer minPixelHeight);
}
