package org.skyve.impl.metadata.view.container.form;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class FormColumn {
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer percentageWidth;

	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}
}
