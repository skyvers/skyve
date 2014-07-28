package org.skyve.wildcat.metadata.view.widget;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"displayName", 
							"dialogName", 
							"command", 
							"dialogWidth", 
							"dialogHeight", 
							"modalDialog",
							"invisibleConditionName", 
							"disabledConditionName", 
							"parameters"})
public class DialogButton implements MetaData, Parameterizable, Disableable, Invisible {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4201233664827983726L;

	private String displayName;
	private String dialogName;
	private String command;
	private Integer dialogWidth = new Integer(800);
	private Integer dialogHeight = new Integer(630);
	private Boolean modalDialog = Boolean.FALSE;
	private String invisibleConditionName;
	private String disabledConditionName;
	private List<Parameter> parameters = new ArrayList<>();

	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(namespace = XMLUtil.VIEW_NAMESPACE,
					name = "parameter",
					type = org.skyve.wildcat.metadata.view.widget.bound.Parameter.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}

	public String getDisplayName() {
		return displayName;
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
	@XmlAttribute(required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	@Override
	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@Override
	@XmlAttribute(required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
}
