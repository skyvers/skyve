package org.skyve.wildcat.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.AbsoluteSize;
import org.skyve.wildcat.metadata.view.ConstrainableHeight;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"pixelWidth", "pixelHeight", "minPixelHeight", "maxPixelHeight"})
public class RichText extends ChangeableInputWidget implements AbsoluteSize, ConstrainableHeight {
	private static final long serialVersionUID = -4873861225052464043L;

	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;

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

	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	@Override
	@XmlAttribute(required = true)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}
}
