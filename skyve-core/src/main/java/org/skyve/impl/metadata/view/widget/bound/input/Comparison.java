package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Editable;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated comparison widget that renders two document instances
 * side-by-side using a named model.
 *
 * <p>Supports relative sizing, editability, and disabled/invisible conditions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"modelName",
							"editable",
							"pixelWidth",
							"responsiveWidth",
							"sm",
							"md",
							"lg",
							"xl",
							"percentageWidth",
							"minPixelWidth",
							"maxPixelWidth",
							"pixelHeight",
							"percentageHeight",
							"minPixelHeight",
							"maxPixelHeight",
							"properties"})
public class Comparison extends InputWidget implements DecoratedMetaData, Editable, RelativeSize {
	private static final long serialVersionUID = -8947146232520711674L;
	
	private String modelName;
	private Boolean editable;

	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	public @Nullable String getModelName() {
		return modelName;
	}
	
	@XmlAttribute(required = false)
	public void setModelName(@Nullable String modelName) {
		this.modelName = UtilImpl.processStringValue(modelName);
	}

	@Override
	public @Nullable Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(@Nullable Boolean editable) {
		this.editable = editable;
	}

	@Override
	public @Nullable Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(@Nullable Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public @Nullable Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(@Nullable Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}

	@Override
	public @Nullable Integer getSm() {
		return sm;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setSm(@Nullable Integer sm) {
		this.sm = sm;
	}

	@Override
	public @Nullable Integer getMd() {
		return md;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setMd(@Nullable Integer md) {
		this.md = md;
	}
	
	@Override
	public @Nullable Integer getLg() {
		return lg;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setLg(@Nullable Integer lg) {
		this.lg = lg;
	}

	@Override
	public @Nullable Integer getXl() {
		return xl;
	}
	
	@Override
	@XmlAttribute(required = false)
	public void setXl(@Nullable Integer xl) {
		this.xl = xl;
	}

	@Override
	public @Nullable Integer getPercentageWidth() {
		return percentageWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(@Nullable Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	@Override
	public @Nullable Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(@Nullable Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	@Override
	public @Nullable Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(@Nullable Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	@Override
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public @Nullable Integer getPercentageHeight() {
		return percentageHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(@Nullable Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	@Override
	public @Nullable Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(@Nullable Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public @Nullable Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(@Nullable Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
}
