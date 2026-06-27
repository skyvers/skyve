package org.skyve.impl.metadata.view.widget.bound;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ContentSpecifiedWidth;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * A button that zooms into an association or inverseOne nominated by the binding.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"displayName", 
							"escapeDisplayName",
							"invisibleConditionName", 
							"visibleConditionName",
							"disabledConditionName", 
							"enabledConditionName",
							"properties"})
public class ZoomIn extends AbstractBound implements Disableable,
														Invisible,
														AbsoluteSize,
														ContentSpecifiedWidth,
														ConstrainableHeight,
														FormItemWidget {
	private static final long serialVersionUID = 100210631435597022L;

	private String displayName;
	private Boolean escapeDisplayName;
	private String relativeIconFileName;
	private String iconStyleClass;
	private String toolTip;
	private Boolean escapeToolTip;
	private Integer pixelWidth;
	private Integer pixelHeight;
	private Integer minPixelHeight;
	private Integer maxPixelHeight;
	private String disabledConditionName;
	private String invisibleConditionName;
	private ActionShow show;

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Indicates that this widget does not render a separate form label by default.
	 *
	 * @return {@code false} because the zoom-in control renders as a standalone button
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the configured button label.
	 *
	 * @return the button label, or {@code null} to use the default label
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * Returns the button label localised for the current user locale.
	 *
	 * <p>Falls back to {@code "Zoom In"} when no explicit display name is configured.
	 *
	 * @return the localised button label
	 */
	public String getLocalisedDisplayName() {
		return Util.nullSafeI18n((displayName == null) ? "Zoom In" : displayName);
	}

	/**
	 * Returns whether the display name text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeDisplayName() {
		return escapeDisplayName;
	}
	
	/**
	 * Sets the button label.
	 *
	 * @param displayName the button label, or {@code null} to use the default label
	 */
	@XmlAttribute
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	/**
	 * Sets whether the display name text should be escaped before rendering.
	 *
	 * @param escapeDisplayName {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@XmlAttribute(required = false)
	public void setEscapeDisplayName(Boolean escapeDisplayName) {
		this.escapeDisplayName = escapeDisplayName;
	}

	/**
	 * Returns the project-relative icon file path for the button.
	 *
	 * @return the project-relative icon file path, or {@code null} when not set
	 */
	public String getRelativeIconFileName() {
		return relativeIconFileName;
	}

	/**
	 * Sets the project-relative icon file path for the button.
	 *
	 * @param relativeIconFileName the project-relative icon file path
	 */
	@XmlAttribute(required = false)
	public void setRelativeIconFileName(String relativeIconFileName) {
		this.relativeIconFileName = UtilImpl.processStringValue(relativeIconFileName);
	}

	/**
	 * Returns the CSS class used to render the button icon.
	 *
	 * @return the icon CSS class, or {@code null} when not set
	 */
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the CSS class used to render the button icon.
	 *
	 * @param iconStyleClass the icon CSS class
	 */
	@XmlAttribute(required = false)
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	/**
	 * Returns the raw tooltip text.
	 *
	 * @return the tooltip text, or {@code null} when not set
	 */
	public String getToolTip() {
		return toolTip;
	}

	/**
	 * Returns the tooltip localised for the current user locale.
	 *
	 * @return the localised tooltip text, or {@code null} when no tooltip is configured
	 */
	public String getLocalisedToolTip() {
		return Util.i18n(toolTip);
	}

	/**
	 * Returns whether the tooltip text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeToolTip() {
		return escapeToolTip;
	}
	
	/**
	 * Sets the tooltip text.
	 *
	 * @param toolTip the tooltip text
	 */
	@XmlAttribute(required = false)
	public void setToolTip(String toolTip) {
		this.toolTip = UtilImpl.processStringValue(toolTip);
	}

	/**
	 * Sets whether the tooltip text should be escaped before rendering.
	 *
	 * @param escapeToolTip {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@XmlAttribute(required = false)
	public void setEscapeToolTip(Boolean escapeToolTip) {
		this.escapeToolTip = escapeToolTip;
	}

	/**
	 * Returns the absolute width of the button in pixels.
	 *
	 * @return the pixel width, or {@code null} when width is not fixed
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute width of the button in pixels.
	 *
	 * @param pixelWidth the pixel width, or {@code null} to clear the fixed width
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the absolute height of the button in pixels.
	 *
	 * @return the pixel height, or {@code null} when height is not fixed
	 */
	@Override
	public Integer getPixelHeight() {
		return pixelHeight;
	}

	/**
	 * Sets the absolute height of the button in pixels.
	 *
	 * @param pixelHeight the pixel height, or {@code null} to clear the fixed height
	 */
	@Override
	@XmlAttribute(required = false)
	public void setPixelHeight(Integer pixelHeight) {
		this.pixelHeight = pixelHeight;
	}

	/**
	 * Returns the minimum button height in pixels.
	 *
	 * @return the minimum pixel height, or {@code null} when no minimum is enforced
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum button height in pixels.
	 *
	 * @param minPixelHeight the minimum pixel height, or {@code null} to clear the minimum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the maximum button height in pixels.
	 *
	 * @return the maximum pixel height, or {@code null} when no maximum is enforced
	 */
	@Override
	public Integer getMaxPixelHeight() {
		return maxPixelHeight;
	}

	/**
	 * Sets the maximum button height in pixels.
	 *
	 * @param maxPixelHeight the maximum pixel height, or {@code null} to clear the maximum
	 */
	@Override
	@XmlAttribute(required = false)
	public void setMaxPixelHeight(Integer maxPixelHeight) {
		this.maxPixelHeight = maxPixelHeight;
	}

	/**
	 * Returns when this action should be shown in the UI.
	 *
	 * @return the display policy for this action, or {@code null} when the default policy applies
	 */
	public ActionShow getShow() {
		return show;
	}

	/**
	 * Sets when this action should be shown in the UI.
	 *
	 * @param show the display policy for this action
	 */
	@XmlAttribute
	public void setShow(ActionShow show) {
		this.show = show;
	}

	/**
	 * Returns the disabled condition expression.
	 *
	 * @return the disabled condition expression, or {@code null} when not set
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disabled condition expression directly.
	 *
	 * @param disabledConditionName the disabled condition expression
	 */
	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}

	/**
	 * JAXB placeholder for the enabled condition attribute.
	 *
	 * @return always {@code null}; the enabled condition is derived from the disabled condition
	 */
	// to enable JAXB XML marshalling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Sets the enabled condition by storing its negation as the disabled condition.
	 *
	 * @param enabledConditionName the enabled condition expression
	 */
	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	/**
	 * Returns the invisible condition expression.
	 *
	 * @return the invisible condition expression, or {@code null} when not set
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible condition expression directly.
	 *
	 * @param invisibleConditionName the invisible condition expression
	 */
	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	/**
	 * JAXB placeholder for the visible condition attribute.
	 *
	 * @return always {@code null}; the visible condition is derived from the invisible condition
	 */
	// to enable JAXB XML marshalling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visible condition by storing its negation as the internal invisible condition.
	 *
	 * @param visibleConditionName the visible condition expression
	 */
	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	/**
	 * Returns the mutable decorator property map for this zoom-in control.
	 *
	 * @return the mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
