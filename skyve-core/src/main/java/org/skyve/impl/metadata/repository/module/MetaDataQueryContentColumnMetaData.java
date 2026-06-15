package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <content>} binary/content column in a
 * metadata query.
 *
 * <p>Specifies the display mode (thumbnail/download), pixel height for thumbnail
 * rendering, and a fallback empty-thumbnail image path.  Extends
 * {@link MetaDataQueryColumnMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see MetaDataQueryColumnMetaData
 * @see MetaDataQueryProjectedColumnMetaData
 */
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "content")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"display", "pixelHeight", "emptyThumbnailRelativeFile"})
public class MetaDataQueryContentColumnMetaData extends MetaDataQueryColumnMetaData {
	private static final long serialVersionUID = 7831641243591117311L;

	/**
	 * Defines how content values are rendered in query results.
	 */
	@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are metadata XML values.
	public enum DisplayType {
		/** Render content as an inline thumbnail preview. */
		thumbnail,
		/** Render content as a link suitable for download/open in a new request. */
		link;
	}
	
	private DisplayType display;
	private Integer pixelHeight;
	private String emptyThumbnailRelativeFile;
	
	/**
	 * Returns the configured content display mode.
	 *
	 * @return the display mode, never {@code null} once metadata validation has completed
	 */
	public DisplayType getDisplay() {
		return display;
	}

	/**
	 * Sets the content display mode.
	 *
	 * @param display the display mode to apply
	 */
	@XmlAttribute(required = true)
	public void setDisplay(DisplayType display) {
		this.display = display;
	}

	/**
	 * Returns the pixel height used for thumbnail rendering.
	 *
	 * @return the configured thumbnail height in pixels, or {@code null} to use the default height
	 */
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the pixel height used for thumbnail rendering.
	 *
	 * @param pixelHeight the thumbnail height in pixels, or {@code null} to use default rendering
	 */
	@XmlAttribute
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the fallback thumbnail resource path used when no thumbnail is available.
	 *
	 * @return the fallback relative resource path, or {@code null} if none is configured
	 */
	public String getEmptyThumbnailRelativeFile() {
		return emptyThumbnailRelativeFile;
	}

	/**
	 * Sets the fallback thumbnail resource path used when no content thumbnail exists.
	 *
	 * <p>Side effects: normalises the supplied value with
	 * {@link UtilImpl#processStringValue(String)} before storing it.
	 *
	 * @param emptyThumbnailRelativeFile the fallback relative resource path; blank values become {@code null}
	 */
	@XmlAttribute
	public void setEmptyThumbnailRelativeFile(String emptyThumbnailRelativeFile) {
		this.emptyThumbnailRelativeFile = UtilImpl.processStringValue(emptyThumbnailRelativeFile);
	}
}
