package org.skyve.impl.metadata.module.query;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;

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
	private Map<String, String> properties = new TreeMap<>();
	
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

	@Override
	public Map<String, String> getProperties() {
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
	public Sanitisation getSanitise() {
		return Sanitisation.none;
	}
}
