package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "spinner")
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"min", "max", "step"})
public class Spinner extends TextField {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5688526602834604710L;

	private Double min;
	private Double max;
	private Double step;

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
	
	public Double getStep() {
		return step;
	}
	@XmlAttribute(required = false)
	public void setStep(Double step) {
		this.step = step;
	}
}
