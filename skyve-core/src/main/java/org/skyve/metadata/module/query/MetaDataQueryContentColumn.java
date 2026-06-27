package org.skyve.metadata.module.query;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;

/**
 * A {@link MetaDataQueryColumn} that displays a content attachment as a thumbnail image
 * or as a downloadable link in list views.
 *
 * <p>The display mode is controlled by {@link #getDisplay()}. When the attachment is
 * absent, an optional placeholder thumbnail image is shown via
 * {@link #getEmptyThumbnailRelativeFile()}.
 *
 * @see MetaDataQueryColumn
 * @see MetaDataQueryProjectedColumn
 */
public interface MetaDataQueryContentColumn extends MetaDataQueryColumn {
	/**
	 * Returns the display mode for this content column (thumbnail or link).
	 *
	 * @return the display type; never {@code null}
	 */
	public DisplayType getDisplay();

	/**
	 * Returns the explicit pixel height for thumbnail display, or {@code null} to use
	 * the default thumbnail height.
	 *
	 * @return the thumbnail height in pixels, or {@code null}
	 */
	public Integer getPixelHeight();

	/**
	 * Returns the context-relative path to the placeholder thumbnail image shown when
	 * no content attachment exists for a row.
	 *
	 * @return the relative path to the empty-state thumbnail, or {@code null} if none
	 */
	public String getEmptyThumbnailRelativeFile();
}
