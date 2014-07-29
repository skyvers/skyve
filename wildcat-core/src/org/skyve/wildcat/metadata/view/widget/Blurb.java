package org.skyve.wildcat.metadata.view.widget;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.metadata.view.AbsoluteSize;
import org.skyve.wildcat.metadata.view.ContentSpecifiedWidth;
import org.skyve.wildcat.metadata.view.HorizontalAlignment;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

/**
 * If a label width/height is not specified, it sizes to fit its contents.
 * 
 * @author mike
 */
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"markup",
							"pixelWidth", 
							"pixelHeight", 
							"textAlignment",
							"invisibleConditionName"})
public class Blurb implements MetaData, Invisible, AbsoluteSize, ContentSpecifiedWidth {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -1234525506006033853L;

	/**
	 * The content to display in the blurb
	 */
	private String markup;

	private Integer pixelWidth;
	private Integer pixelHeight;
	
	private String invisibleConditionName;

	/**
	 * Default alignment is left.
	 */
	private HorizontalAlignment textAlignment = null;
	
	public String getMarkup() {
		return markup;
	}

	@XmlValue
	public void setMarkup(String markup) {
		this.markup = UtilImpl.processStringValue(markup);
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

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	public HorizontalAlignment getTextAlignment() {
		return textAlignment;
	}

	@XmlAttribute(name = "textAlignment", required = false)
	public void setTextAlignment(HorizontalAlignment textAlignment) {
		this.textAlignment = textAlignment;
	}
}
