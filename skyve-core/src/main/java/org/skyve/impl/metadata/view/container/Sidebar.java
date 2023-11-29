package org.skyve.impl.metadata.view.container;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Invisible;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "sidebar")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "sidebar",
			propOrder = {"widgetId",
							"floatingPixelWidth",
							"floatingPixelWidthBreakpoint",
							"pixelWidth",
							"percentageWidth",
							"responsiveWidth"})
public class Sidebar extends Container implements Identifiable, Invisible, RelativeWidth {
	private static final long serialVersionUID = 7637506523705376564L;

	private String widgetId;
	private Integer floatingPixelWidth;
	private Integer floatingPixelWidthBreakpoint;
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer responsiveWidth;

	private String invisibleConditionName;

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
	
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	public Integer getFloatingPixelWidth() {
		return floatingPixelWidth;
	}

	@XmlAttribute(required = false)
	public void setFloatingPixelWidth(Integer floatingPixelWidth) {
		this.floatingPixelWidth = floatingPixelWidth;
	}

	public Integer getFloatingPixelWidthBreakpoint() {
		return floatingPixelWidthBreakpoint;
	}

	@XmlAttribute(required = true)
	public void setFloatingPixelWidthBreakpoint(Integer floatingPixelWidthBreakpoint) {
		this.floatingPixelWidthBreakpoint = floatingPixelWidthBreakpoint;
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
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}
}
