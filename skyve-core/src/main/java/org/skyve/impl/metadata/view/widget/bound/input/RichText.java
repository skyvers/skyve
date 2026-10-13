package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.TextOutput.Sanitisation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated rich-text (HTML) input widget bound to a Long-text String
 * attribute.
 *
 * <p>Renders a WYSIWYG rich-text editor with configurable dimensions and
 * optional content sanitisation.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see HTML
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight", "minPixelHeight", "maxPixelHeight", "sanitise", "properties"})
public class RichText extends ChangeableInputWidget implements AbsoluteSize, ConstrainableHeight, FormItemWidget {
	private static final long serialVersionUID = -4873861225052464043L;

	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	private Sanitisation sanitise;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	@Override
	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}
	
	@Override
	@XmlAttribute(required = true)
	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}
	
	@Override
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}
	
	@Override
	@XmlAttribute(required = true)
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public @Nullable Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setMinPixelHeight(@Nullable Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public @Nullable Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setMaxPixelHeight(@Nullable Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	public @Nullable Sanitisation getSanitise() {
		return sanitise;
	}

	@XmlAttribute
	public void setSanitise(@Nullable Sanitisation sanitise) {
		this.sanitise = sanitise;
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
