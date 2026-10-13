package org.skyve.impl.metadata.view.container;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Invisible;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
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

	private String invisibleCondition;
	private String visibleCondition;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	/**
	 * Returns the derived {@code invisible} condition that renderers and other consumers read.
	 *
	 * @return the {@code invisible} attribute as authored, or else the negation of the authored
	 * 			{@code visible} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getInvisibleCondition() {
		return (invisibleCondition != null) ? invisibleCondition : BindUtil.negateCondition(visibleCondition);
	}

	/**
	 * Sets the {@code invisible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code visible} attribute.
	 *
	 * @param invisibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setXmlInvisibleCondition(@Nullable String invisibleCondition) {
		this.invisibleCondition = UtilImpl.processStringValue(invisibleCondition);
		this.visibleCondition = null;
	}

	/**
	 * Returns the {@code invisible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code visible} or neither was authored
	 */
	@Override
	public @Nullable String getXmlInvisibleCondition() {
		return invisibleCondition;
	}

	/**
	 * Returns the {@code visible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code invisible} or neither was authored
	 */
	@Override
	public @Nullable String getXmlVisibleCondition() {
		return visibleCondition;
	}

	/**
	 * Sets the {@code visible} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code invisible} attribute, so that
	 * {@link #getInvisibleCondition()} returns the negation of this condition.
	 *
	 * @param visibleCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setXmlVisibleCondition(@Nullable String visibleCondition) {
		this.visibleCondition = UtilImpl.processStringValue(visibleCondition);
		this.invisibleCondition = null;
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
