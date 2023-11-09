package org.skyve.impl.metadata.view.container;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "sidebar")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, name = "sidebar", propOrder = { "widgetId",
		"title",
		"floatingPixelWidth",
		"floatingPercentageWidth",
		"floatingResponsiveWidth",
		"floatingPixelWidthBreakpoint",
		"pixelWidth",
		"percentageWidth",
		"responsiveWidth" })
public class Sidebar extends Container implements Identifiable, Invisible {
	private static final long serialVersionUID = 7637506523705376564L;

	private String widgetId;
	private String title;
	private Integer floatingPixelWidth;
	private Integer floatingPercentageWidth;
	private Integer floatingResponsiveWidth;
	private Integer floatingPixelWidthBreakpoint;
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer responsiveWidth;

	private String invisibleConditionName;
	private HorizontalAlignment horizontalAlignment;
	private VerticalAlignment verticalAlignment;

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
	
	public HorizontalAlignment getHorizontalAlignment() {
		return horizontalAlignment;
	}

	@XmlAttribute(required = false)
	public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
		this.horizontalAlignment = horizontalAlignment;
	}
	
	public VerticalAlignment getVerticalAlignment() {
		return verticalAlignment;
	}

	@XmlAttribute(required = false)
	public void setVerticalAlignment(VerticalAlignment verticalAlignment) {
		this.verticalAlignment = verticalAlignment;
	}

	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	public String getTitle() {
		return title;
	}

	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}
	
	public String getLocalisedTitle() {
		return Util.i18n(title);
	}

	public Integer getFloatingPixelWidth() {
		return floatingPixelWidth;
	}

	@XmlAttribute(required = false)
	public void setFloatingPixelWidth(Integer floatingPixelWidth) {
		this.floatingPixelWidth = floatingPixelWidth;
	}

	public Integer getFloatingPercentageWidth() {
		return floatingPercentageWidth;
	}

	@XmlAttribute(required = false)
	public void setFloatingPercentageWidth(Integer floatingPercentageWidth) {
		this.floatingPercentageWidth = floatingPercentageWidth;
	}

	public Integer getFloatingResponsiveWidth() {
		return floatingResponsiveWidth;
	}

	@XmlAttribute(required = false)
	public void setFloatingResponsiveWidth(Integer floatingResponsiveWidth) {
		this.floatingResponsiveWidth = floatingResponsiveWidth;
	}

	public Integer getFloatingPixelWidthBreakpoint() {
		return floatingPixelWidthBreakpoint;
	}

	@XmlAttribute(required = true)
	public void setFloatingPixelWidthBreakpoint(Integer floatingPixelWidthBreakpoint) {
		this.floatingPixelWidthBreakpoint = floatingPixelWidthBreakpoint;
	}

	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}
}
