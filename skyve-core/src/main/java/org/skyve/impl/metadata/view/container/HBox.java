package org.skyve.impl.metadata.view.container;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Bordered;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated horizontal-box container widget.
 *
 * <p>Arranges child widgets in a horizontal row with optional border and
 * widget identifier.  Extends {@link Container} so child widgets can be
 * nested using the standard JAXB element-ref polymorphism.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see VBox
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, name = "HBox")
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "hbox")
public class HBox extends Container implements Box, Identifiable, Bordered {
	private static final long serialVersionUID = 5014102577585857713L;

	private String widgetId;
	
	private Boolean border;
	private String borderTitle;
	private Boolean escapeBorderTitle;
	
	private Integer pixelWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer percentageWidth;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private VerticalAlignment verticalAlignment;
	private HorizontalAlignment horizontalAlignment;

	private Collapsible collapsible;
	
	private Integer pixelPadding;
	private Integer pixelMemberPadding;
	private ShrinkWrap shrinkWrap;
	
	private String invisibleConditionName;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the widget identifier for this container.
	 *
	 * @return widget identifier, or {@code null} when not configured
	 */
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	/**
	 * Sets the widget identifier for this container.
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
	 * Returns whether a border is rendered around this container.
	 *
	 * @return {@code true} when a border is requested, {@code false} when disabled, or {@code null} when unspecified
	 */
	@Override
	public Boolean getBorder() {
		return border;
	}

	/**
	 * Sets whether a border is rendered around this container.
	 *
	 * @param border  border flag; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "border", required = false)
	public void setBorder(Boolean border) {
		this.border = border;
	}

	/**
	 * Returns the optional border title.
	 *
	 * @return border title, or {@code null} when not configured
	 */
	@Override
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Returns whether the border title text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@Override
	public Boolean getEscapeBorderTitle() {
		return escapeBorderTitle;
	}

	/**
	 * Sets the optional border title.
	 *
	 * @param borderTitle  border title; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "borderTitle", required = false)
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
	}

	/**
	 * Sets whether the border title text should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@Override
	@XmlAttribute(name = "escapeBorderTitle", required = false)
	public void setEscapeBorderTitle(Boolean escapeBorderTitle) {
		this.escapeBorderTitle = escapeBorderTitle;
	}

	/**
	 * Returns the configured percentage height.
	 *
	 * @return percentage height, or {@code null} when unset
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the configured percentage height.
	 *
	 * @param percentageHeight  percentage height value; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "percentageHeight", required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
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
	@XmlAttribute(name = "percentageWidth", required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
	}

	/**
	 * Returns the configured fixed pixel height.
	 *
	 * @return pixel height, or {@code null} when unset
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the configured fixed pixel height.
	 *
	 * @param pixelHeight  pixel height value; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "pixelHeight", required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the minimum pixel height constraint.
	 *
	 * @return minimum pixel height, or {@code null} when unset
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height constraint.
	 *
	 * @param minPixelHeight  minimum pixel height; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum pixel height constraint.
	 *
	 * @return maximum pixel height, or {@code null} when unset
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum pixel height constraint.
	 *
	 * @param maxPixelHeight  maximum pixel height; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns the configured fixed pixel width.
	 *
	 * @return pixel width, or {@code null} when unset
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the configured fixed pixel width.
	 *
	 * @param pixelWidth  pixel width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "pixelWidth", required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
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
	 * Returns the configured small-breakpoint width.
	 *
	 * @return small-breakpoint width, or {@code null} when unset
	 */
	@Override
	public Integer getSm() {
		return sm;
	}
	
	/**
	 * Sets the configured small-breakpoint width.
	 *
	 * @param sm  small-breakpoint width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setSm(Integer sm) {
		this.sm = sm;
	}

	/**
	 * Returns the configured medium-breakpoint width.
	 *
	 * @return medium-breakpoint width, or {@code null} when unset
	 */
	@Override
	public Integer getMd() {
		return md;
	}
	
	/**
	 * Sets the configured medium-breakpoint width.
	 *
	 * @param md  medium-breakpoint width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMd(Integer md) {
		this.md = md;
	}
	
	/**
	 * Returns the configured large-breakpoint width.
	 *
	 * @return large-breakpoint width, or {@code null} when unset
	 */
	@Override
	public Integer getLg() {
		return lg;
	}
	
	/**
	 * Sets the configured large-breakpoint width.
	 *
	 * @param lg  large-breakpoint width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setLg(Integer lg) {
		this.lg = lg;
	}

	/**
	 * Returns the configured extra-large-breakpoint width.
	 *
	 * @return extra-large-breakpoint width, or {@code null} when unset
	 */
	@Override
	public Integer getXl() {
		return xl;
	}
	
	/**
	 * Sets the configured extra-large-breakpoint width.
	 *
	 * @param xl  extra-large-breakpoint width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setXl(Integer xl) {
		this.xl = xl;
	}

	/**
	 * Returns the minimum pixel width constraint.
	 *
	 * @return minimum pixel width, or {@code null} when unset
	 */
	@Override
	public Integer getMinPixelWidth() {
		return minPixelWidth;
	}

	/**
	 * Sets the minimum pixel width constraint.
	 *
	 * @param minPixelWidth  minimum pixel width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelWidth(Integer minPixelWidth) {
		this.minPixelWidth = minPixelWidth;
	}

	/**
	 * Returns the maximum pixel width constraint.
	 *
	 * @return maximum pixel width, or {@code null} when unset
	 */
	@Override
	public Integer getMaxPixelWidth() {
		return maxPixelWidth;
	}

	/**
	 * Sets the maximum pixel width constraint.
	 *
	 * @param maxPixelWidth  maximum pixel width; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelWidth(Integer maxPixelWidth) {
		this.maxPixelWidth = maxPixelWidth;
	}

	/**
	 * Returns the pixel padding applied around this container.
	 *
	 * @return pixel padding, or {@code null} when unset
	 */
	@Override
	public Integer getPixelPadding() {
		return pixelPadding;
	}

	/**
	 * Sets the pixel padding applied around this container.
	 *
	 * @param pixelPadding  pixel padding value; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "pixelPadding", required = false)
	public void setPixelPadding(Integer pixelPadding) {
		this.pixelPadding = pixelPadding;
	}

	/**
	 * Returns the pixel spacing between child members.
	 *
	 * @return member padding in pixels, or {@code null} when unset
	 */
	@Override
	public Integer getPixelMemberPadding() {
		return pixelMemberPadding;
	}

	/**
	 * Sets the pixel spacing between child members.
	 *
	 * @param pixelMemberPadding  member padding in pixels; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "pixelMemberPadding", required = false)
	public void setPixelMemberPadding(Integer pixelMemberPadding) {
		this.pixelMemberPadding = pixelMemberPadding;
	}

	/**
	 * Returns the vertical alignment for child content.
	 *
	 * @return vertical alignment, or {@code null} when renderer defaults are used
	 */
	public VerticalAlignment getVerticalAlignment() {
		return verticalAlignment;
	}

	/**
	 * Sets the vertical alignment for child content.
	 *
	 * @param verticalAlignment  vertical alignment to apply; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setVerticalAlignment(VerticalAlignment verticalAlignment) {
		this.verticalAlignment = verticalAlignment;
	}

	/**
	 * Returns the horizontal alignment for child content.
	 *
	 * @return horizontal alignment, or {@code null} when renderer defaults are used
	 */
	public HorizontalAlignment getHorizontalAlignment() {
		return horizontalAlignment;
	}

	/**
	 * Sets the horizontal alignment for child content.
	 *
	 * @param horizontalAlignment  horizontal alignment to apply; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setHorizontalAlignment(HorizontalAlignment horizontalAlignment) {
		this.horizontalAlignment = horizontalAlignment;
	}

	/**
	 * Returns the collapse behavior for this container.
	 *
	 * @return collapse mode, or {@code null} when collapsibility is not configured
	 */
	@Override
	public Collapsible getCollapsible() {
		return collapsible;
	}

	/**
	 * Sets the collapse behavior for this container.
	 *
	 * @param collapsible  collapse mode to apply; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setCollapsible(Collapsible collapsible) {
		this.collapsible = collapsible;
	}

	/**
	 * Returns the shrink-wrap behavior for this container.
	 *
	 * @return shrink-wrap mode, or {@code null} when not configured
	 */
	@Override
	public ShrinkWrap getShrinkWrap() {
		return shrinkWrap;
	}

	/**
	 * Sets the shrink-wrap behavior for this container.
	 *
	 * @param shrinkWrap  shrink-wrap mode to apply; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setShrinkWrap(ShrinkWrap shrinkWrap) {
		this.shrinkWrap = shrinkWrap;
	}

	/**
	 * Returns the invisible-condition expression for this container.
	 *
	 * @return invisible-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this container.
	 *
	 * @param invisibleConditionName  invisible-condition expression; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	/**
	 * Placeholder getter used only for JAXB compatibility of the synthetic visible attribute.
	 *
	 * @return always {@code null}
	 */
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
	 * Returns the decorator property map for this container.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
