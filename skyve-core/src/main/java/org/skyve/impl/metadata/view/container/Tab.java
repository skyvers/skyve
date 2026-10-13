package org.skyve.impl.metadata.view.container;

import java.util.LinkedHashMap;
import java.util.Map;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.util.Util;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
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
							"xmlDisabledCondition", 
							"xmlEnabledCondition", 
							"xmlInvisibleCondition", 
							"xmlVisibleCondition", 
							"properties"})
public final class Tab extends Container implements Disableable, Invisible, DecoratedMetaData {
	private static final long serialVersionUID = -3216551162394859248L;

	private String title;
	private Boolean escapeTitle;
	private String icon16x16RelativeFileName;
	private String iconStyleClass;
	private String disabledCondition;
	private String enabledCondition;
	private String invisibleCondition;
	private String visibleCondition;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

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
	 * Returns the decorator property map for this tab.
	 *
	 * @return a mutable property map; never {@code null}
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
