package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;

public class MetaDataQueryContentColumnImpl extends AbstractMetaDataQueryColumn implements MetaDataQueryContentColumn {
	private static final long serialVersionUID = 8798725861664551014L;

	private DisplayType display;
	private Integer pixelHeight;
	private String emptyThumbnailRelativeFile;
	
	@Override
	public DisplayType getDisplay() {
		return display;
	}
	
	public void setDisplay(DisplayType display) {
		this.display = display;
	}
	
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}
	
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}
	
	@Override
	public String getEmptyThumbnailRelativeFile() {
		return emptyThumbnailRelativeFile;
	}

	public void setEmptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		this.emptyThumbnailRelativeFile = emptyThumbnailRelativeFile;
	}
}
