package org.skyve.impl.metadata.view.container;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated sidebar container widget for a view.
 *
 * <p>A sidebar is a collateral panel rendered alongside the main content area.
 * It supports relative width, visibility conditions, a widget identifier,
 * and decorator properties.  Child widgets are held via {@link Container}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "sidebar")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "sidebar",
			propOrder = {"widgetId",
							"floatingPixelWidth",
							"floatingPixelWidthBreakpoint",
							"pixelWidth",
							"percentageWidth",
							"responsiveWidth",
							"properties"})
public class Sidebar extends Container implements Identifiable, Invisible, RelativeWidth, DecoratedMetaData {
	private static final long serialVersionUID = 7637506523705376564L;

	private String widgetId;
	private Integer floatingPixelWidth;
	private Integer floatingPixelWidthBreakpoint;
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer responsiveWidth;

	private String invisibleConditionName;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the invisible-condition expression for this sidebar.
	 *
	 * @return invisible-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this sidebar.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param invisibleConditionName  invisible-condition expression; may be {@code null} or blank
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	/**
	 * Placeholder getter used only for JAXB compatibility of the synthetic visible attribute.
	 *
	 * @return always {@code null}
	 */
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Applies a positive visibility expression by storing its negated form.
	 *
	 * <p>Side effects: overwrites the value returned by {@link #getInvisibleConditionName()}.
	 *
	 * @param visibleConditionName  the visibility expression from metadata; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}
	
	/**
	 * Returns the widget identifier for this sidebar.
	 *
	 * @return widget identifier, or {@code null} when not configured
	 */
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	/**
	 * Sets the widget identifier for this sidebar.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param widgetId  widget identifier; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	/**
	 * Returns the width in pixels used when the sidebar is rendered as a floating panel.
	 *
	 * @return the floating width in pixels, or {@code null} to use renderer defaults
	 */
	public Integer getFloatingPixelWidth() {
		return floatingPixelWidth;
	}

	/**
	 * Sets the width in pixels used when the sidebar is rendered as a floating panel.
	 *
	 * @param floatingPixelWidth  floating width in pixels; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setFloatingPixelWidth(Integer floatingPixelWidth) {
		this.floatingPixelWidth = floatingPixelWidth;
	}

	/**
	 * Returns the responsive breakpoint that activates floating sidebar behavior.
	 *
	 * @return a breakpoint width in pixels, or {@code null} when not configured
	 */
	public Integer getFloatingPixelWidthBreakpoint() {
		return floatingPixelWidthBreakpoint;
	}

	/**
	 * Sets the responsive breakpoint that activates floating sidebar behavior.
	 *
	 * @param floatingPixelWidthBreakpoint  breakpoint width in pixels
	 */
	@XmlAttribute(required = true)
	public void setFloatingPixelWidthBreakpoint(Integer floatingPixelWidthBreakpoint) {
		this.floatingPixelWidthBreakpoint = floatingPixelWidthBreakpoint;
	}

	/**
	 * Returns the fixed pixel width for this sidebar.
	 *
	 * @return pixel width, or {@code null} when unset
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the fixed pixel width for this sidebar.
	 *
	 * @param pixelWidth  pixel width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the configured percentage width.
	 *
	 * @return percentage width, or {@code null} when unset
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the configured percentage width.
	 *
	 * @param percentageWidth  percentage width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the configured responsive width.
	 *
	 * @return responsive width value, or {@code null} when unset
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the configured responsive width.
	 *
	 * @param responsiveWidth  responsive width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setResponsiveWidth(Integer responsiveWidth) {
		this.responsiveWidth = responsiveWidth;
	}
	
	/**
	 * Returns the decorator property map for this sidebar.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
