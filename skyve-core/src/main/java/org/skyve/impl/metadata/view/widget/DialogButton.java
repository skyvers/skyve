package org.skyve.impl.metadata.view.widget;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.FormItemWidget;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.util.Util;

import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated button widget that opens a named dialog or executes a
 * dialog command.
 *
 * <p>Binds a labelled button to a named dialog, passing optional parameters.
 * Supports configurable dialog dimensions, modal display, disabled/invisible
 * conditions.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"displayName", 
							"escapeDisplayName",
							"dialogName", 
							"command", 
							"dialogWidth", 
							"dialogHeight", 
							"modalDialog",
							"xmlInvisibleCondition", 
							"xmlVisibleCondition",
							"xmlDisabledCondition", 
							"xmlEnabledCondition",
							"parameters",
							"properties"})
public class DialogButton implements Parameterizable, Disableable, Invisible, FormItemWidget {
	private static final long serialVersionUID = 4201233664827983726L;

	private String displayName;
	private Boolean escapeDisplayName;
	private String dialogName;
	private String command;
	private Integer dialogWidth = Integer.valueOf(800);
	private Integer dialogHeight = Integer.valueOf(630);
	private Boolean modalDialog = Boolean.FALSE;
	private String invisibleCondition;
	private String visibleCondition;
	private String disabledCondition;
	private String enabledCondition;
	private List<Parameter> parameters = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	/**
	 * Indicates that this widget does not render a separate form label by default.
	 *
	 * @return {@code false} because the dialog button renders its own label
	 */
	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	/**
	 * Returns the mutable parameter list passed when opening the dialog.
	 *
	 * @return the mutable dialog parameter list; never {@code null}
	 */
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Returns the configured button label.
	 *
	 * @return the button label
	 */
	public String getDisplayName() {
		return displayName;
	}

	/**
	 * Returns the display label localised for the current user locale.
	 *
	 * @return the localised display label
	 */
	public String getLocalisedDisplayName() {
		return Util.i18n(displayName);
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
	 * @param displayName the button label
	 */
	@XmlAttribute(required = true)
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
	 * Returns the name of the dialog to open.
	 *
	 * @return the dialog name, or {@code null} when a command is used instead
	 */
	public String getDialogName() {
		return dialogName;
	}
	
	/**
	 * Sets the name of the dialog to open.
	 *
	 * @param dialogName the dialog name, or {@code null} when a command is used instead
	 */
	@XmlAttribute(required = false)
	public void setDialogName(String dialogName) {
		this.dialogName = UtilImpl.processStringValue(dialogName);
	}

	/**
	 * Returns the dialog command to execute.
	 *
	 * @return the dialog command, or {@code null} when opening a named dialog directly
	 */
	public String getCommand() {
		return command;
	}

	/**
	 * Sets the dialog command to execute.
	 *
	 * @param command the dialog command, or {@code null} when opening a named dialog directly
	 */
	@XmlAttribute(required = false)
	public void setCommand(String command) {
		this.command = UtilImpl.processStringValue(command);
	}

	/**
	 * Returns the dialog width in pixels.
	 *
	 * @return the dialog width in pixels
	 */
	public Integer getDialogWidth() {
		return dialogWidth;
	}

	/**
	 * Sets the dialog width in pixels.
	 *
	 * @param dialogWidth the dialog width in pixels
	 */
	@XmlAttribute(required = false)
	public void setDialogWidth(Integer dialogWidth) {
		this.dialogWidth = dialogWidth;
	}

	/**
	 * Returns the dialog height in pixels.
	 *
	 * @return the dialog height in pixels
	 */
	public Integer getDialogHeight() {
		return dialogHeight;
	}

	/**
	 * Sets the dialog height in pixels.
	 *
	 * @param dialogHeight the dialog height in pixels
	 */
	@XmlAttribute(required = false)
	public void setDialogHeight(Integer dialogHeight) {
		this.dialogHeight = dialogHeight;
	}

	/**
	 * Returns whether the dialog should be shown modally.
	 *
	 * @return {@code true} when the dialog is modal
	 */
	public Boolean isModalDialog() {
		return modalDialog;
	}

	/**
	 * Sets whether the dialog should be shown modally.
	 *
	 * @param modalDialog {@code true} to open the dialog modally
	 */
	@XmlAttribute(required = false)
	public void setModalDialog(Boolean modalDialog) {
		this.modalDialog = modalDialog;
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
	 * Returns the mutable decorator property map for this dialog button.
	 *
	 * @return the mutable decorator property map keyed by property name
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
