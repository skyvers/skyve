package org.skyve.wildcat.metadata.view.container;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.view.Invisible;
import org.skyve.wildcat.metadata.Container;
import org.skyve.wildcat.metadata.view.Bordered;
import org.skyve.wildcat.metadata.view.Identifiable;
import org.skyve.wildcat.metadata.view.RelativeSize;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE, name = "HBox")
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "hbox")
public class HBox extends Container implements Box, Identifiable, RelativeSize, Invisible, Bordered {
	/**
	 * For Serialization.
	 */
	private static final long serialVersionUID = 5014102577585857713L;

	private String widgetId;
	
	private Boolean border;
	private String borderTitle;
	
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer pixelHeight;
	private Integer percentageHeight;
	
	private Integer pixelPadding;
	private Integer pixelMemberPadding;
	
	private String invisibleConditionName;

	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	@Override
	public Boolean getBorder() {
		return border;
	}

	@Override
	@XmlAttribute(name = "border", required = false)
	public void setBorder(Boolean border) {
		this.border = border;
	}

	@Override
	public String getBorderTitle() {
		return borderTitle;
	}

	@Override
	@XmlAttribute(name = "borderTitle", required = false)
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	@Override
	@XmlAttribute(name = "percentageHeight", required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
	}

	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@Override
	@XmlAttribute(name = "percentageWidth", required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	@Override
	@XmlAttribute(name = "pixelHeight", required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(name = "pixelWidth", required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	@Override
	public Integer getPixelPadding() {
		return pixelPadding;
	}

	@Override
	@XmlAttribute(name = "pixelPadding", required = false)
	public void setPixelPadding(Integer pixelPadding) {
		this.pixelPadding = pixelPadding;
	}

	@Override
	public Integer getPixelMemberPadding() {
		return pixelMemberPadding;
	}

	@Override
	@XmlAttribute(name = "pixelMemberPadding", required = false)
	public void setPixelMemberPadding(Integer pixelMemberPadding) {
		this.pixelMemberPadding = pixelMemberPadding;
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}
}
