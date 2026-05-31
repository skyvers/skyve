package org.skyve.impl.metadata.view.container.form;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Bordered;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated form layout container widget.
 *
 * <p>Renders an aligned grid of labelled input fields using {@link FormRow} and
 * {@link FormColumn} children.  Supports relative sizing, borders, visibility
 * and disability conditions, a widget identifier, and decorator properties.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see FormRow
 * @see FormColumn
 * @see FormItem
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"widgetId",
							"pixelWidth", 
							"responsiveWidth",
							"sm",
							"md",
							"lg",
							"xl",
							"percentageWidth", 
							"minPixelWidth", 
							"maxPixelWidth", 
							"pixelHeight", 
							"percentageHeight", 
							"minPixelHeight", 
							"maxPixelHeight", 
							"border",
							"borderTitle",
							"collapsible",
							"labelDefaultHorizontalAlignment",
							"labelLayout",
							"disabledConditionName",
							"enabledConditionName",
							"invisibleConditionName", 
							"visibleConditionName",
							"columns", 
							"rows",
							"properties"})
public final class Form implements DecoratedMetaData, Identifiable, RelativeSize, Disableable, Invisible, Bordered {
	private static final long serialVersionUID = 8677483284773272582L;

	private String widgetId;
	
	private Boolean border;
	private String borderTitle;
	
	private Integer pixelWidth;
	private Integer percentageWidth;
	private Integer responsiveWidth;
	private Integer sm;
	private Integer md;
	private Integer lg;
	private Integer xl;
	private Integer minPixelWidth;
	private Integer maxPixelWidth;

	private Integer pixelHeight;
	private Integer percentageHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	
	private HorizontalAlignment labelDefaultHorizontalAlignment;
	
	private Collapsible collapsible;
	
	private FormLabelLayout labelLayout;

	private String disabledConditionName;
	private String invisibleConditionName;
	
	private List<FormColumn> columns = new ArrayList<>();
	private List<FormRow> rows = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the live ordered column definitions for this form.
	 *
	 * <p>Side effects: mutating the returned list mutates this metadata instance.
	 *
	 * @return a mutable list of column definitions; never {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "column", required = true)
	public List<FormColumn> getColumns() {
		return columns;
	}

	/**
	 * Returns the live ordered row definitions for this form.
	 *
	 * <p>Side effects: mutating the returned list mutates this metadata instance.
	 *
	 * @return a mutable list of rows; never {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "row", required = true)
	public List<FormRow> getRows() {
		return rows;
	}

	/**
	 * Returns the widget identifier for this form.
	 *
	 * @return the widget identifier, or {@code null} when not configured
	 */
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	/**
	 * Sets the widget identifier for this form.
	 *
	 * @param widgetId  identifier value to store; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	/**
	 * Returns whether a border is rendered around this form.
	 *
	 * @return {@code true} when a border is requested, {@code false} when disabled, or {@code null} when unspecified
	 */
	@Override
	public Boolean getBorder() {
		return border;
	}

	/**
	 * Sets whether a border is rendered around this form.
	 *
	 * @param border  border flag; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setBorder(Boolean border) {
		this.border = border;
	}

	/**
	 * Returns the border title text.
	 *
	 * @return border title text, or {@code null} when not configured
	 */
	@Override
	public String getBorderTitle() {
		return borderTitle;
	}

	/**
	 * Sets the border title text.
	 *
	 * @param borderTitle  border title text; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setBorderTitle(String borderTitle) {
		this.borderTitle = borderTitle;
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
	@XmlAttribute(required = false)
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
	@XmlAttribute(required = false)
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
	@XmlAttribute(required = false)
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
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the configured responsive width value.
	 *
	 * @return responsive width value, or {@code null} when unset
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the configured responsive width value.
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
	 * Returns the default horizontal alignment for form labels.
	 *
	 * <p>When unset, renderers use their module or platform default label alignment.
	 *
	 * @return the default label alignment, or {@code null} when no override is defined
	 */
	public HorizontalAlignment getLabelDefaultHorizontalAlignment() {
		return labelDefaultHorizontalAlignment;
	}

	/**
	 * Sets the default horizontal alignment for form labels.
	 *
	 * @param labelDefaultHorizontalAlignment  default label alignment to apply; may be {@code null}
	 */
	@XmlAttribute(name = "defaultLabelAlign", required = false)
	public void setLabelDefaultHorizontalAlignment(HorizontalAlignment labelDefaultHorizontalAlignment) {
		this.labelDefaultHorizontalAlignment = labelDefaultHorizontalAlignment;
	}
	
	/**
	 * Returns the collapse behavior for this form container.
	 *
	 * @return the collapse mode, or {@code null} when collapsibility is not configured
	 */
	public Collapsible getCollapsible() {
		return collapsible;
	}

	/**
	 * Sets the collapse behavior for this form container.
	 *
	 * @param collapsible  collapse mode to apply; may be {@code null}
	 */
	@XmlAttribute(required = false)
	public void setCollapsible(Collapsible collapsible) {
		this.collapsible = collapsible;
	}

	/**
	 * Returns the label layout strategy used by this form.
	 *
	 * @return the label layout strategy, or {@code null} to use module defaults
	 */
	public FormLabelLayout getLabelLayout() {
		return labelLayout;
	}

	/**
	 * Sets the label layout strategy used by this form.
	 *
	 * @param labelLayout  label layout strategy to apply; may be {@code null}
	 */
	@XmlAttribute(name = "labelLayout", required = false)
	public void setLabelLayout(FormLabelLayout labelLayout) {
		this.labelLayout = labelLayout;
	}

	/**
	 * Returns the disabled-condition expression for this form.
	 *
	 * @return disabled-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disabled-condition expression for this form.
	 *
	 * @param disabledConditionName  disabled-condition expression; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	// to enable JAXB XML marshaling
	/**
	 * Placeholder getter used only for JAXB compatibility of the synthetic enabled attribute.
	 *
	 * @return always {@code null}
	 */
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Applies a positive enabled expression by storing its negated disabled form.
	 *
	 * <p>Side effects: overwrites the value returned by {@link #getDisabledConditionName()}.
	 *
	 * @param enabledConditionName  the enabled expression from metadata; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	/**
	 * Returns the invisible-condition expression for this form.
	 *
	 * @return invisible-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this form.
	 *
	 * @param invisibleConditionName  invisible-condition expression; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
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
	 * Applies a positive visibility expression by storing its negated invisible form.
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
	 * Returns the decorator property map for this form.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
