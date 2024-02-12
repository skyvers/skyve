package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "spinner")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"min", "max", "step"})
public class Spinner extends TextField {
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
