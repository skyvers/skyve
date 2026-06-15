package org.skyve.impl.metadata.view.container;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.RelativeSize;
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
 * JAXB-annotated tabbed-panel container widget.
 *
 * <p>Renders as a multi-tab panel with an ordered list of {@link Tab} children.
 * Supports relative sizing, disabled/invisible conditions, a widget identifier,
 * and decorator properties.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see Tab
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
							"disabledConditionName", 
							"enabledConditionName",
							"invisibleConditionName", 
							"visibleConditionName",
							"selectedTabIndexBinding",
							"tabs",
							"properties"})
public final class TabPane implements DecoratedMetaData, Identifiable, RelativeSize, Disableable, Invisible {
	private static final long serialVersionUID = -3490366758123216975L;

	private String widgetId;
	
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
	
	private String disabledConditionName;
	private String invisibleConditionName;
	private String selectedTabIndexBinding;
	private List<Tab> tabs = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the widget identifier for this tab pane.
	 *
	 * @return widget identifier, or {@code null} when not configured
	 */
	@Override
	public String getWidgetId() {
		return widgetId;
	}

	/**
	 * Sets the widget identifier for this tab pane.
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
	 * Returns the live ordered tab list for this pane.
	 *
	 * <p>Side effects: mutating the returned list mutates this metadata instance.
	 *
	 * @return a mutable tab list; never {@code null}
	 */
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "tab", required = true)
	public List<Tab> getTabs() {
		return tabs;
	}

	/**
	 * Returns the fixed pixel width for this tab pane.
	 *
	 * @return pixel width, or {@code null} when unset
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the fixed pixel width for this tab pane.
	 *
	 * @param pixelWidth  pixel width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the responsive width for this tab pane.
	 *
	 * @return responsive width value, or {@code null} when unset
	 */
	@Override
	public Integer getResponsiveWidth() {
		return responsiveWidth;
	}

	/**
	 * Sets the responsive width for this tab pane.
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
	 * Returns the percentage width for this tab pane.
	 *
	 * @return percentage width, or {@code null} when unset
	 */
	@Override
	public Integer getPercentageWidth() {
		return percentageWidth;
	}

	/**
	 * Sets the percentage width for this tab pane.
	 *
	 * @param percentageWidth  percentage width value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageWidth(Integer percentageWidth) {
		this.percentageWidth = percentageWidth;
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
	 * Returns the fixed pixel height for this tab pane.
	 *
	 * @return pixel height, or {@code null} when unset
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the fixed pixel height for this tab pane.
	 *
	 * @param pixelHeight  pixel height value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the percentage height for this tab pane.
	 *
	 * @return percentage height, or {@code null} when unset
	 */
	@Override
	public Integer getPercentageHeight() {
		return percentageHeight;
	}

	/**
	 * Sets the percentage height for this tab pane.
	 *
	 * @param percentageHeight  percentage height value; may be {@code null}
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPercentageHeight(Integer percentageHeight) {
		this.percentageHeight = percentageHeight;
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
	 * Returns the disabled-condition expression for this tab pane.
	 *
	 * @return disabled-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disabled-condition expression for this tab pane.
	 *
	 * @param disabledConditionName  disabled-condition expression; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = disabledConditionName;
	}

	/**
	 * Placeholder getter used only for JAXB compatibility of the synthetic enabled attribute.
	 *
	 * @return always {@code null}
	 */
	// to enable JAXB XML marshaling
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
	 * Returns the invisible-condition expression for this tab pane.
	 *
	 * @return invisible-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this tab pane.
	 *
	 * @param invisibleConditionName  invisible-condition expression; may be {@code null}
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = invisibleConditionName;
	}

	/**
	 * Placeholder getter used only for JAXB compatibility of the synthetic visible attribute.
	 *
	 * @return always {@code null}
	 */
	// to enable JAXB XML marshaling
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
	 * Returns the binding expression that tracks the selected tab index.
	 *
	 * @return a binding expression, or {@code null} when no selection binding is configured
	 */
	public String getSelectedTabIndexBinding() {
		return selectedTabIndexBinding;
	}

	/**
	 * Sets the binding expression that tracks the selected tab index.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param selectedTabIndexBinding  binding expression to store; may be {@code null} or blank
	 */
	@XmlAttribute(required = false)
	public void setSelectedTabIndexBinding(String selectedTabIndexBinding) {
		this.selectedTabIndexBinding = UtilImpl.processStringValue(selectedTabIndexBinding);
	}

	/**
	 * Returns the decorator property map for this tab pane.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
