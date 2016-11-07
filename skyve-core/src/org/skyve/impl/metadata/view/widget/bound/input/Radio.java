package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.widget.bound.input.ChangeableInputWidget;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public final class Radio extends ChangeableInputWidget implements AbsoluteWidth {
	private static final long serialVersionUID = -6194966991364516589L;
	
	private Boolean vertical;
	private Integer pixelWidth;

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}
	
	public Boolean getVertical() {
		return vertical;
	}

	@XmlAttribute(required = false)
	public void setVertical(Boolean vertical) {
		this.vertical = vertical;
	}
}
