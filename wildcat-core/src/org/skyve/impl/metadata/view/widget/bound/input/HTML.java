package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight"})
public class HTML extends InputWidget implements AbsoluteSize {
	/**
	 * For Serialzation
	 */
	private static final long serialVersionUID = -2155059200252882977L;

	private Integer pixelWidth;
	private Integer pixelHeight;

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(required = true)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}
}
