package org.skyve.impl.metadata.view.widget;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.MetaData;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public class Spacer implements AbsoluteSize, MetaData {
	private static final long serialVersionUID = -3535525299887373582L;

	private Integer pixelWidth;
	private Integer pixelHeight;
	
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}
}
