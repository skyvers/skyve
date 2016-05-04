package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight", "triState"})
public class CheckBox extends ChangeableInputWidget implements AbsoluteSize {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6260632670896362783L;
	
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Boolean triState;

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

	public Boolean getTriState() {
		return triState;
	}

	@XmlAttribute(required = false)
	public void setTriState(Boolean triState) {
		this.triState = triState;
	}
}
