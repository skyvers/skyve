package org.skyve.impl.metadata.module.query;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Implementation of a content (file) column in a metadata query.
 *
 * <p>Extends {@link AbstractMetaDataQueryColumn} to represent a link or thumbnail
 * to a binary content item attached to the queried record.  The
 * {@code displayType} controls whether the column renders as a download link
 * or an inline image thumbnail.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractMetaDataQueryColumn
 * @see MetaDataQueryContentColumn
 */
public class MetaDataQueryContentColumnImpl extends AbstractMetaDataQueryColumn implements MetaDataQueryContentColumn {
	private static final long serialVersionUID = 8798725861664551014L;

	private DisplayType display;
	private Integer pixelHeight;
	private String emptyThumbnailRelativeFile;
	private Map<String, String> properties = new LinkedHashMap<>();
	
	@Override
	public @Nonnull DisplayType getDisplay() {
		return display;
	}
	
	public void setDisplay(@Nonnull DisplayType display) {
		this.display = display;
	}
	
	@Override
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}
	
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}
	
	@Override
	public @Nullable String getEmptyThumbnailRelativeFile() {
		return emptyThumbnailRelativeFile;
	}

	public void setEmptyThumbnailRelativeFile(@Nullable String emptyThumbnailRelativeFile) {
		this.emptyThumbnailRelativeFile = emptyThumbnailRelativeFile;
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Don't escape content columns
	 */
	@Override
	public boolean isEscape() {
		return false;
	}

	/**
	 * Don't sanitise content columns
	 */
	@Override
	public @Nonnull Sanitisation getSanitise() {
		return Sanitisation.none;
	}
}
