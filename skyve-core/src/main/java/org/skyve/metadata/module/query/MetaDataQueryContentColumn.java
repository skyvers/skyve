package org.skyve.metadata.module.query;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;

/**
 * A column that can display content as a thumbnail or a link.
 */
public interface MetaDataQueryContentColumn extends MetaDataQueryColumn {
	public DisplayType getDisplay();
	public Integer getPixelHeight();
	public String getEmptyThumbnailRelativeFile();
}
