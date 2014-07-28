package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.AbsoluteWidth;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class TextField extends ChangeableInputWidget implements AbsoluteWidth {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -5061565177091806441L;

	private Integer pixelWidth;
	private Boolean previousValues;
	
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

	@XmlAttribute
	public void setPreviousValues(Boolean previousValues) {
		this.previousValues = previousValues;
	}
}
