package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Signature-capture widget bound to a content attribute.
 *
 * <p>Allows users to draw a signature in the browser and persists the resulting
 * image data into the bound content field. Supports explicit canvas dimensions
 * plus optional foreground and background colour overrides.
 *
 * <p>Threading: not thread-safe. Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth",
							"pixelHeight",
							"rgbHexBackgroundColour",
							"rgbHexForegroundColour",
							"properties"})
public class ContentSignature extends InputWidget implements AbsoluteSize, FormItemWidget {
	private static final long serialVersionUID = 7902784327466913291L;
	
	private Integer pixelWidth;
	private Integer pixelHeight;
	private String rgbHexBackgroundColour;
	private String rgbHexForegroundColour;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that this widget renders its label by default when placed in a form.
	 *
	 * @return {@code true} because signature widgets are presented as standard
	 *         labelled form items
	 */
	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	/**
	 * Returns the configured signature canvas width in pixels.
	 *
	 * @return the explicit pixel width, or {@code null} when the client default
	 *         width should be used
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the signature canvas width in pixels.
	 *
	 * @param pixelWidth the explicit pixel width, or {@code null} to defer to the
	 *        client default
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the configured signature canvas height in pixels.
	 *
	 * @return the explicit pixel height, or {@code null} when the client default
	 *         height should be used
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the signature canvas height in pixels.
	 *
	 * @param pixelHeight the explicit pixel height, or {@code null} to defer to
	 *        the client default
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the implementation-specific widget properties.
	 *
	 * <p>The returned map is mutable and live, allowing callers or JAXB to add
	 * vendor-specific client options directly to this widget definition.
	 *
	 * @return a mutable property map, never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Returns the signature pad background colour override.
	 *
	 * @return an RGB hex colour (for example {@code #FFFFFF}), or {@code null}
	 *         to use the client default
	 */
	public String getRgbHexBackgroundColour() {
		return rgbHexBackgroundColour;
	}

	/**
	 * Sets the signature pad background colour override.
	 *
	 * @param rgbHexBackgroundColour an RGB hex colour (for example
	 *        {@code #FFFFFF}), or {@code null} to use the client default
	 */
	@XmlAttribute(required = false)
	public void setRgbHexBackgroundColour(String rgbHexBackgroundColour) {
		this.rgbHexBackgroundColour = rgbHexBackgroundColour;
	}
	
	/**
	 * Returns the signature stroke colour override.
	 *
	 * @return an RGB hex colour (for example {@code #000000}), or {@code null}
	 *         to use the client default
	 */
	public String getRgbHexForegroundColour() {
		return rgbHexForegroundColour;
	}

	/**
	 * Sets the signature stroke colour override.
	 *
	 * @param rgbHexForegroundColour an RGB hex colour (for example
	 *        {@code #000000}), or {@code null} to use the client default
	 */
	@XmlAttribute(required = false)
	public void setRgbHexForegroundColour(String rgbHexForegroundColour) {
		this.rgbHexForegroundColour = rgbHexForegroundColour;
	}	
}
