package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;

public class FluentMetaDataQueryContentColumn extends FluentMetaDataQueryColumn<FluentMetaDataQueryContentColumn> {
	private MetaDataQueryContentColumnMetaData column = null;
	
	public FluentMetaDataQueryContentColumn() {
		column = new MetaDataQueryContentColumnMetaData();
	}
	
	public FluentMetaDataQueryContentColumn(MetaDataQueryContentColumnMetaData column) {
		this.column = column;
	}	

	public FluentMetaDataQueryContentColumn from(@SuppressWarnings("hiding") MetaDataQueryContentColumn column) {
		super.from(column);
		display(column.getDisplay());
		Integer i = column.getPixelHeight();
		if (i != null) {
			pixelHeight(i.intValue());
		}
		emptyThumbnailRelativeFile(column.getEmptyThumbnailRelativeFile());
		return this;
	}
	
	public FluentMetaDataQueryContentColumn display(DisplayType display) {
		column.setDisplay(display);
		return this;
	}
	
	public FluentMetaDataQueryContentColumn pixelHeight(int pixelHeight) {
		column.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}
	
	public FluentMetaDataQueryContentColumn emptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		column.setEmptyThumbnailRelativeFile(emptyThumbnailRelativeFile);
		return this;
	}

	@Override
	public MetaDataQueryContentColumnMetaData get() {
		return column;
	}
}
