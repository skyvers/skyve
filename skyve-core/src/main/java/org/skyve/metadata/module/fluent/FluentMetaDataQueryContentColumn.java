package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;

/**
 * Builds metadata query content column definitions.
 */
public class FluentMetaDataQueryContentColumn extends FluentMetaDataQueryColumn<FluentMetaDataQueryContentColumn> {
	private MetaDataQueryContentColumnMetaData column = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMetaDataQueryContentColumn() {
		column = new MetaDataQueryContentColumnMetaData();
	}
	
	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param column The metadata to mutate.
	 */
	public FluentMetaDataQueryContentColumn(MetaDataQueryContentColumnMetaData column) {
		this.column = column;
	}	

	/**
	 * Copies content-column fields from an existing definition.
	 *
	 * @param column The source content column definition.
	 * @return this fluent instance.
	 */
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
	
	/**
	 * Sets how content is rendered in this column.
	 *
	 * @param display The content display type.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryContentColumn display(DisplayType display) {
		column.setDisplay(display);
		return this;
	}
	
	/**
	 * Sets the fixed pixel height for content rendering.
	 *
	 * @param pixelHeight The pixel height.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryContentColumn pixelHeight(int pixelHeight) {
		column.setPixelHeight(Integer.valueOf(pixelHeight));
		return this;
	}
	
	/**
	 * Sets the fallback thumbnail path for empty content.
	 *
	 * @param emptyThumbnailRelativeFile The relative thumbnail file path.
	 * @return this fluent instance.
	 */
	public FluentMetaDataQueryContentColumn emptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		column.setEmptyThumbnailRelativeFile(emptyThumbnailRelativeFile);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The content column metadata instance.
	 */
	@Override
	public MetaDataQueryContentColumnMetaData get() {
		return column;
	}
}
