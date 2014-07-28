package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.AbsoluteSize;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "slider")
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"min", 
							"max", 
							"numberOfDiscreteValues", 
							"roundingPrecision", 
							"vertical", 
							"pixelWidth", 
							"pixelHeight"})
public class Slider extends ChangeableInputWidget implements AbsoluteSize {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = -1215719151652616337L;

	private Double min;
	private Double max;
	private Integer numberOfDiscreteValues;
	private Integer roundingPrecision;
	private Boolean vertical;
	private Integer pixelWidth;
	private Integer pixelHeight;
	
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
}
