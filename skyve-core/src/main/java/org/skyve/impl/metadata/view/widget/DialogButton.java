package org.skyve.impl.metadata.view.widget;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

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

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"displayName", 
							"dialogName", 
							"command", 
							"dialogWidth", 
							"dialogHeight", 
							"modalDialog",
							"invisibleConditionName", 
							"visibleConditionName",
							"disabledConditionName", 
							"enabledConditionName",
							"parameters",
							"properties"})
public class DialogButton implements Parameterizable, Disableable, Invisible, FormItemWidget {
	private static final long serialVersionUID = 4201233664827983726L;

	private String displayName;
	private String dialogName;
	private String command;
	private Integer dialogWidth = Integer.valueOf(800);
	private Integer dialogHeight = Integer.valueOf(630);
	private Boolean modalDialog = Boolean.FALSE;
	private String invisibleConditionName;
	private String disabledConditionName;
	private List<Parameter> parameters = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public boolean showsLabelByDefault() {
		return false;
	}
	
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	public String getDisplayName() {
		return displayName;
	}

	public String getLocalisedDisplayName() {
		return Util.i18n(dialogName);
	}
	
	@XmlAttribute(required = true)
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	public String getDialogName() {
		return dialogName;
	}
	
	@XmlAttribute(required = false)
	public void setDialogName(String dialogName) {
		this.dialogName = UtilImpl.processStringValue(dialogName);
	}

	public String getCommand() {
		return command;
	}

	@XmlAttribute(required = false)
	public void setCommand(String command) {
		this.command = UtilImpl.processStringValue(command);
	}

	public Integer getDialogWidth() {
		return dialogWidth;
	}

	@XmlAttribute(required = false)
	public void setDialogWidth(Integer dialogWidth) {
		this.dialogWidth = dialogWidth;
	}

	public Integer getDialogHeight() {
		return dialogHeight;
	}

	@XmlAttribute(required = false)
	public void setDialogHeight(Integer dialogHeight) {
		this.dialogHeight = dialogHeight;
	}

	public Boolean isModalDialog() {
		return modalDialog;
	}

	@XmlAttribute(required = false)
	public void setModalDialog(Boolean modalDialog) {
		this.modalDialog = modalDialog;
	}

	@Override
	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@Override
	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	@Override
	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
