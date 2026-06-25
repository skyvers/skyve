package org.skyve.impl.metadata.view.container;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated tab descriptor within a {@link TabPane}.
 *
 * <p>Holds a titled tab containing a {@link Container} of child widgets.
 * Supports disabled and invisible conditions as well as decorator properties.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see TabPane
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, 
			propOrder = {"title",
							"escapeTitle",
							"icon16x16RelativeFileName", 
							"iconStyleClass", 
							"disabledConditionName", 
							"enabledConditionName", 
							"invisibleConditionName", 
							"visibleConditionName", 
							"properties"})
public final class Tab extends Container implements Disableable, Invisible, DecoratedMetaData {
	private static final long serialVersionUID = -3216551162394859248L;

	private String title;
	private Boolean escapeTitle;
	private String icon16x16RelativeFileName;
	private String iconStyleClass;
	private String disabledConditionName;
	private String invisibleConditionName;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the configured tab title.
	 *
	 * @return tab title, or {@code null} when not configured
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * Returns the tab title translated for the current locale.
	 *
	 * @return the localised title, or {@code null} when no title is configured
	 */
	public String getLocalisedTitle() {
		return Util.i18n(title);
	}

	/**
	 * Returns whether the tab title text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeTitle() {
		return escapeTitle;
	}
	
	/**
	 * Sets the tab title.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param title  tab title; may be {@code null} or blank
	 */
	@XmlAttribute(required = true)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	/**
	 * Sets whether the tab title text should be escaped before rendering.
	 *
	 * @param escapeTitle {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@XmlAttribute(required = false)
	public void setEscapeTitle(Boolean escapeTitle) {
		this.escapeTitle = escapeTitle;
	}

	/**
	 * Returns the relative path to the tab icon resource.
	 *
	 * @return icon path, or {@code null} when not configured
	 */
	public String getIcon16x16RelativeFileName() {
		return icon16x16RelativeFileName;
	}

	/**
	 * Sets the relative path to the tab icon resource.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param icon16x16RelativeFileName  relative icon path; may be {@code null} or blank
	 */
	@XmlAttribute(name = "icon16x16RelativeFileName")
	public void setIcon16x16RelativeFileName(String icon16x16RelativeFileName) {
		this.icon16x16RelativeFileName = UtilImpl.processStringValue(icon16x16RelativeFileName);
	}

	/**
	 * Returns the icon CSS style class for this tab.
	 *
	 * @return icon style class, or {@code null} when not configured
	 */
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the icon CSS style class for this tab.
	 *
	 * <p>Side effects: trims and normalises the supplied value before storage.
	 *
	 * @param iconStyleClass  icon style class; may be {@code null} or blank
	 */
	@XmlAttribute(name = "iconStyleClass")
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	/**
	 * Returns the disabled-condition expression for this tab.
	 *
	 * @return disabled-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the disabled-condition expression for this tab.
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
	 * Returns the invisible-condition expression for this tab.
	 *
	 * @return invisible-condition expression, or {@code null} when not configured
	 */
	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the invisible-condition expression for this tab.
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
	 * Returns the decorator property map for this tab.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
