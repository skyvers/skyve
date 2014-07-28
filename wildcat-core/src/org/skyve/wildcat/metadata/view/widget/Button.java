package org.skyve.wildcat.metadata.view.widget;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.metadata.view.AbsoluteSize;
import org.skyve.wildcat.metadata.view.ContentSpecifiedWidth;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * If button width/height is not specified it sizes to fit it's contents - the label and icon.
 * 
 * @author mike
 */
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"actionName", 
							"pixelWidth", 
							"pixelHeight"})
public class Button implements MetaData, AbsoluteSize, ContentSpecifiedWidth {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2344473519207771461L;

	private String actionName;
	private Integer pixelWidth;
	private Integer pixelHeight;

	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
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

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}
}
