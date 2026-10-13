package org.skyve.impl.metadata.view.container.form;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
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
							"escapeBorderTitle",
							"collapsible",
							"labelDefaultHorizontalAlignment",
							"labelLayout",
							"xmlDisabledCondition",
							"xmlEnabledCondition",
							"xmlInvisibleCondition", 
							"xmlVisibleCondition",
							"columns", 
							"rows",
							"properties"})
public final class Form implements DecoratedMetaData, Identifiable, RelativeSize, Disableable, Invisible, Bordered {
	private static final long serialVersionUID = 8677483284773272582L;

	private String widgetId;
	
	private Boolean border;
	private String borderTitle;
	private Boolean escapeBorderTitle;
	
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

	private String disabledCondition;
	private String enabledCondition;
	private String invisibleCondition;
	private String visibleCondition;
	
	private List<FormColumn> columns = new ArrayList<>();
	private List<FormRow> rows = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

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
	 * Returns whether the border title text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@Override
	public Boolean getEscapeBorderTitle() {
		return escapeBorderTitle;
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
	 * Sets whether the border title text should be escaped before rendering.
	 *
	 * @param escapeBorderTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@Override
	@XmlAttribute(required = false)
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
	 * Returns the derived {@code disabled} condition that renderers and other consumers read.
	 *
	 * @return the {@code disabled} attribute as authored, or else the negation of the authored
	 * 			{@code enabled} attribute; {@code null} when neither was authored
	 */
	@Override
	@XmlTransient
	public @Nullable String getDisabledCondition() {
		return (disabledCondition != null) ? disabledCondition : BindUtil.negateCondition(enabledCondition);
	}

	/**
	 * Sets the {@code disabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code enabled} attribute.
	 *
	 * @param disabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setXmlDisabledCondition(@Nullable String disabledCondition) {
		this.disabledCondition = UtilImpl.processStringValue(disabledCondition);
		this.enabledCondition = null;
	}

	/**
	 * Returns the {@code disabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code enabled} or neither was authored
	 */
	@Override
	public @Nullable String getXmlDisabledCondition() {
		return disabledCondition;
	}

	/**
	 * Returns the {@code enabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disabled} or neither was authored
	 */
	@Override
	public @Nullable String getXmlEnabledCondition() {
		return enabledCondition;
	}

	/**
	 * Sets the {@code enabled} attribute as authored.
	 *
	 * <p>Side effects: clears any authored {@code disabled} attribute, so that
	 * {@link #getDisabledCondition()} returns the negation of this condition.
	 *
	 * @param enabledCondition a condition name, an inline condition expression or a boolean literal; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setXmlEnabledCondition(@Nullable String enabledCondition) {
		this.enabledCondition = UtilImpl.processStringValue(enabledCondition);
		this.disabledCondition = null;
	}

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
	 * Returns the decorator property map for this form.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
