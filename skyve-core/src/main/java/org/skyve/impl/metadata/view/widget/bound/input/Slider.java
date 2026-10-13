package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated slider input widget bound to a numeric attribute.
 *
 * <p>Renders a horizontal or vertical drag slider with configurable minimum,
 * maximum, step count, rounding precision, and pixel dimensions.  Supports
 * absolute sizing, change events, and disabled/invisible conditions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "slider")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"min", 
							"max", 
							"numberOfDiscreteValues", 
							"roundingPrecision", 
							"vertical", 
							"pixelWidth", 
							"pixelHeight",
							"minPixelHeight",
							"maxPixelHeight",
							"properties"})
public class Slider extends ChangeableInputWidget implements AbsoluteSize, ConstrainableHeight, FormItemWidget {
	private static final long serialVersionUID = -1215719151652616337L;

	private Double min;
	private Double max;
	private Integer numberOfDiscreteValues;
	private Integer roundingPrecision;
	private Boolean vertical;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public @Nullable Double getMin() {
		return min;
	}
	@XmlAttribute(required = false)
	public void setMin(@Nullable Double min) {
		this.min = min;
	}
	
	public @Nullable Double getMax() {
		return max;
	}
	@XmlAttribute(required = false)
	public void setMax(@Nullable Double max) {
		this.max = max;
	}
	
	public @Nullable Integer getNumberOfDiscreteValues() {
		return numberOfDiscreteValues;
	}
	@XmlAttribute(required = false)
	public void setNumberOfDiscreteValues(@Nullable Integer numberOfDiscreteValues) {
		this.numberOfDiscreteValues = numberOfDiscreteValues;
	}
	
	public @Nullable Integer getRoundingPrecision() {
		return roundingPrecision;
	}
	@XmlAttribute(required = false)
	public void setRoundingPrecision(@Nullable Integer roundingPrecision) {
		this.roundingPrecision = roundingPrecision;
	}

	public @Nullable Boolean getVertical() {
		return vertical;
	}
	@XmlAttribute(required = false)
	public void setVertical(@Nullable Boolean vertical) {
		this.vertical = vertical;
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
	public @Nullable Integer getPixelHeight() {
		return pixelHeight;
	}
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(@Nullable Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
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
