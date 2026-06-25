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

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
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
							"disabledConditionName",
							"enabledConditionName",
							"invisibleConditionName",
							"visibleConditionName",
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
	private String disabledConditionName;
	private String invisibleConditionName;

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
	 * Returns the internal invisible-condition name.
	 *
	 * @return invisible-condition name, or {@code null}
	 */
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	/**
	 * Sets the internal invisible-condition name.
	 *
	 * @param invisibleConditionName invisible-condition name
	 */
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	/**
	 * Sets the visibility condition using a positive expression.
	 *
	 * <p>Side effects: stores the negated form of {@code visibleConditionName} in
	 * {@code invisibleConditionName} to preserve the internal invisible-condition
	 * representation used by runtime metadata.
	 *
	 * @param visibleConditionName condition indicating when the action is visible
	 */
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	/**
	 * Returns the internal disabled-condition name.
	 *
	 * @return disabled-condition name, or {@code null}
	 */
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	/**
	 * Sets the internal disabled-condition name.
	 *
	 * @param disabledConditionName disabled-condition name
	 */
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	/**
	 * Sets the enabled condition using a positive expression.
	 *
	 * <p>Side effects: stores the negated form of {@code enabledConditionName} in
	 * {@code disabledConditionName} to preserve the internal disabled-condition
	 * representation used by runtime metadata.
	 *
	 * @param enabledConditionName condition indicating when the action is enabled
	 */
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
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
		result.setDisabledConditionName(disabledConditionName);
		result.setDisplayName(displayName);
		result.setEscapeDisplayName(escapeDisplayName);
		result.setName(name);
		if (implicitName != null) {
			result.setImplicitName(implicitName);
			if (name == null) {
				result.setName(implicitName.toString());
			}
		}
		result.setInvisibleConditionName(getInvisibleConditionName());
		result.setRelativeIconFileName(getRelativeIconFileName());
		result.setIconStyleClass(getIconStyleClass());
		result.setToolTip(getToolTip());
		result.setEscapeToolTip(getEscapeToolTip());
		result.setProperties(properties);

		return result;
	}
}
