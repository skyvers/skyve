package org.skyve.impl.metadata.view.container.form;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a column definition within a {@link Form} layout.
 *
 * <p>Declares the pixel width of a data-entry column in the form grid, used by
 * the rendering engine to allocate horizontal space.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see Form
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class FormColumn implements DecoratedMetaData {
	private static final long serialVersionUID = -2927913876248576162L;

	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	public @Nullable Integer getPercentageWidth() {
		return percentageWidth;
	}

	@XmlAttribute(required = false)
	public void setPercentageWidth(@Nullable Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute(required = false)
	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public @Nullable Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	@XmlAttribute(required = false)
	public void setResponsiveWidth(@Nullable Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	public @Nullable Integer getSm() {
		return sm;
	}
	
	@XmlAttribute(required = false)
	public void setSm(@Nullable Integer sm) {
		this.sm = sm;
	}

	public @Nullable Integer getMd() {
		return md;
	}
	
	@XmlAttribute(required = false)
	public void setMd(@Nullable Integer md) {
		this.md = md;
	}
	
	public @Nullable Integer getLg() {
		return lg;
	}
	
	@XmlAttribute(required = false)
	public void setLg(@Nullable Integer lg) {
		this.lg = lg;
	}

	public @Nullable Integer getXl() {
		return xl;
	}
	
	@XmlAttribute(required = false)
	public void setXl(@Nullable Integer xl) {
		this.xl = xl;
	}
	
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
