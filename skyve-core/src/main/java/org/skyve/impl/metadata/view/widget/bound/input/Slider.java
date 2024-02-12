package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

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
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return true;
	}
	
	public Double getMin() {
		return min;
	}
	@XmlAttribute(required = false)
	public void setMin(Double min) {
		this.min = min;
	}
	
	public Double getMax() {
		return max;
	}
	@XmlAttribute(required = false)
	public void setMax(Double max) {
		this.max = max;
	}
	
	public Integer getNumberOfDiscreteValues() {
		return numberOfDiscreteValues;
	}
	@XmlAttribute(required = false)
	public void setNumberOfDiscreteValues(Integer numberOfDiscreteValues) {
		this.numberOfDiscreteValues = numberOfDiscreteValues;
	}
	
	public Integer getRoundingPrecision() {
		return roundingPrecision;
	}
	@XmlAttribute(required = false)
	public void setRoundingPrecision(Integer roundingPrecision) {
		this.roundingPrecision = roundingPrecision;
	}

	public Boolean getVertical() {
		return vertical;
	}
	@XmlAttribute(required = false)
	public void setVertical(Boolean vertical) {
		this.vertical = vertical;
	}

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
