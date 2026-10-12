package org.skyve.impl.metadata.repository.view.actions;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Abstract JAXB base for all action descriptor elements in a view descriptor.
 *
 * <p>Carries the common action properties: name, display name, tooltip, icon,
 * confirmation text, and visibility/disability condition names.  Concrete
 * subclasses ({@link PositionableAction}, {@link ValidatableAction}, etc.) add
 * type-specific properties.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see PositionableAction
 * @see ValidatableAction
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "abstractAction",
			propOrder = {"name", 
							"displayName",
							"escapeDisplayName",
							"toolTip", 
							"escapeToolTip",
							"relativeIconFileName", 
							"iconStyleClass", 
							"confirmationText",
							"escapeConfirm",
							"xmlDisabledCondition",
							"xmlEnabledCondition",
							"xmlInvisibleCondition",
							"xmlVisibleCondition",
							"properties"})
public abstract class ActionMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = -6621003946775667744L;

	protected ImplicitActionName implicitName;

	private String name;
	private String displayName;
	private Boolean escapeDisplayName;
	private String toolTip;
	private Boolean escapeToolTip;
	private String relativeIconFileName;
	private String iconStyleClass;
	private String confirmationText;
	private Boolean escapeConfirm;
	private String disabledCondition;
	private String enabledCondition;
	private String invisibleCondition;
	private String visibleCondition;

	/**
	 * Returns the implicit action name for framework-provided actions.
	 *
	 * @return implicit action name, or {@code null}
	 */
	public ImplicitActionName getImplicitName() {
		return implicitName;
	}

	/**
	 * Returns the explicit action name.
	 *
	 * @return explicit action name, or {@code null}
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the explicit action name.
	 *
	 * @param name explicit action name
	 */
	@XmlAttribute(required = false)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	/**
	 * Returns the display label shown for this action.
	 *
	 * @return display label, or {@code null}
	 */
	public String getDisplayName() {
		return displayName;
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
	 * Sets the display label shown for this action.
	 *
	 * @param displayName display label
	 */
	@XmlAttribute(required = false)
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
	 * Returns the relative icon file name for this action.
	 *
	 * @return relative icon file name, or {@code null}
	 */
	public String getRelativeIconFileName() {
		return relativeIconFileName;
	}

	/**
	 * Sets the relative icon file name for this action.
	 *
	 * @param relativeIconFileName relative icon file name
	 */
	@XmlAttribute(required = false)
	public void setRelativeIconFileName(String relativeIconFileName) {
		this.relativeIconFileName = UtilImpl.processStringValue(relativeIconFileName);
	}

	/**
	 * Returns the icon style class for this action.
	 *
	 * @return icon CSS style class, or {@code null}
	 */
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the icon style class for this action.
	 *
	 * @param iconStyleClass icon CSS style class
	 */
	@XmlAttribute(required = false)
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	/**
	 * Returns the tooltip text for this action.
	 *
	 * @return tooltip text, or {@code null}
	 */
	public String getToolTip() {
		return toolTip;
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
	 * Sets the tooltip text for this action.
	 *
	 * @param toolTip tooltip text
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
	 * Returns confirmation text shown before action execution.
	 *
	 * @return confirmation text, or {@code null}
	 */
	public String getConfirmationText() {
		return confirmationText;
	}

	/**
	 * Returns whether the confirmation text should be escaped before rendering.
	 *
	 * @return {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	public Boolean getEscapeConfirm() {
		return escapeConfirm;
	}

	/**
	 * Sets confirmation text shown before action execution.
	 *
	 * @param confirmationText confirmation text
	 */
	@XmlAttribute(name = "confirm", required = false)
	public void setConfirmationText(String confirmationText) {
		this.confirmationText = UtilImpl.processStringValue(confirmationText);
	}

	/**
	 * Sets whether the confirmation text should be escaped before rendering.
	 *
	 * @param escapeConfirm {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 */
	@XmlAttribute(name = "escapeConfirm", required = false)
	public void setEscapeConfirm(Boolean escapeConfirm) {
		this.escapeConfirm = escapeConfirm;
	}

	/**
	 * Returns the derived {@code invisible} condition that renderers and other consumers read.
	 *
	 * @return the {@code invisible} attribute as authored, or else the negation of the authored
	 * 			{@code visible} attribute; {@code null} when neither was authored
	 */
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
	public @Nullable String getXmlInvisibleCondition() {
		return invisibleCondition;
	}

	/**
	 * Returns the {@code visible} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code invisible} or neither was authored
	 */
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
	@XmlAttribute(name = "visible", required = false)
	public void setXmlVisibleCondition(@Nullable String visibleCondition) {
		this.visibleCondition = UtilImpl.processStringValue(visibleCondition);
		this.invisibleCondition = null;
	}

	/**
	 * Returns the derived {@code disabled} condition that renderers and other consumers read.
	 *
	 * @return the {@code disabled} attribute as authored, or else the negation of the authored
	 * 			{@code enabled} attribute; {@code null} when neither was authored
	 */
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
	public @Nullable String getXmlDisabledCondition() {
		return disabledCondition;
	}

	
	/**
	 * Returns the {@code enabled} attribute as authored.
	 *
	 * @return the authored condition, or {@code null} when {@code disabled} or neither was authored
	 */
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
	@XmlAttribute(name = "enabled", required = false)
	public void setXmlEnabledCondition(@Nullable String enabledCondition) {
		this.enabledCondition = UtilImpl.processStringValue(enabledCondition);
		this.disabledCondition = null;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	@SuppressWarnings("java:S1948") // TreeMap is Serializable
	public Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns decorator properties defined for this action descriptor.
	 *
	 * @return mutable action property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Converts this JAXB action descriptor into runtime action metadata.
	 *
	 * <p>Copies common display, icon, condition, and confirmation properties and
	 * applies implicit action naming defaults when explicit names are omitted.
	 *
	 * @return a populated runtime action instance
	 */
	public ActionImpl toMetaDataAction() {
		ActionImpl result = new ActionImpl();

		result.setConfirmationText(confirmationText);
		result.setEscapeConfirm(escapeConfirm);
		BindUtil.copyAuthoredCondition(getXmlDisabledCondition(), getXmlEnabledCondition(), result::setXmlDisabledCondition, result::setXmlEnabledCondition);
		result.setDisplayName(displayName);
		result.setEscapeDisplayName(escapeDisplayName);
		result.setName(name);
		if (implicitName != null) {
			result.setImplicitName(implicitName);
			if (name == null) {
				result.setName(implicitName.toString());
			}
		}
		BindUtil.copyAuthoredCondition(getXmlInvisibleCondition(), getXmlVisibleCondition(), result::setXmlInvisibleCondition, result::setXmlVisibleCondition);
		result.setRelativeIconFileName(getRelativeIconFileName());
		result.setIconStyleClass(getIconStyleClass());
		result.setToolTip(getToolTip());
		result.setEscapeToolTip(getEscapeToolTip());
		result.setProperties(properties);

		return result;
	}
}
