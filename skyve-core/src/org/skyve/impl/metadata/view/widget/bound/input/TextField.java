package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Editable;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"editable", "previousValues", "pixelWidth"})
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextField extends ChangeableInputWidget implements Editable, AbsoluteWidth {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5061565177091806441L;

	private Boolean editable;
	private Boolean previousValues;
	private Integer pixelWidth;
	
	@Override
	public Boolean getEditable() {
		return editable;
	}

	@Override
	@XmlAttribute(name = "editable", required = false)
	public void setEditable(Boolean editable) {
		this.editable = editable;
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

	public Boolean getPreviousValues() {
		return previousValues;
	}

	@XmlAttribute(required = false)
	public void setPreviousValues(Boolean previousValues) {
		this.previousValues = previousValues;
	}
}
