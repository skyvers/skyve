package org.skyve.wildcat.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			name = "abstractAction",
			propOrder = {"name", 
							"displayName",
							"toolTip", 
							"relativeIconFileName", 
							"confirmationText",
							"disabledConditionName",
							"invisibleConditionName"})
public abstract class Action {
	protected ImplicitActionName implicitName;

	private String name;
	private String displayName;
	private String toolTip;
	private String relativeIconFileName;
	private String confirmationText;
	private String disabledConditionName;
	private String invisibleConditionName;

	public ImplicitActionName getImplicitName() {
		return implicitName;
	}

	public String getName() {
		return name;
	}

	@XmlAttribute(required = false)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	public String getDisplayName() {
		return displayName;
	}

	@XmlAttribute(required = false)
	public void setDisplayName(String displayName) {
		this.displayName = UtilImpl.processStringValue(displayName);
	}

	public String getRelativeIconFileName() {
		return relativeIconFileName;
	}

	@XmlAttribute(required = false)
	public void setRelativeIconFileName(String relativeIconFileName) {
		this.relativeIconFileName = UtilImpl.processStringValue(relativeIconFileName);
	}

	public String getToolTip() {
		return toolTip;
	}

	@XmlAttribute(required = false)
	public void setToolTip(String toolTip) {
		this.toolTip = UtilImpl.processStringValue(toolTip);
	}

	public String getConfirmationText() {
		return confirmationText;
	}

	@XmlAttribute(name = "confirm", required = false)
	public void setConfirmationText(String confirmationText) {
		this.confirmationText = UtilImpl.processStringValue(confirmationText);
	}

	public String getInvisibleConditionName() {
		return invisibleConditionName;
	}

	@XmlAttribute(name = "invisible", required = false)
	public void setInvisibleConditionName(String invisibleConditionName) {
		this.invisibleConditionName = UtilImpl.processStringValue(invisibleConditionName);
	}

	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
	
	public org.skyve.wildcat.metadata.view.Action toMetaDataAction() {
		org.skyve.wildcat.metadata.view.Action result = new org.skyve.wildcat.metadata.view.Action();

		result.setConfirmationText(confirmationText);
		result.setDisabledConditionName(disabledConditionName);
		result.setDisplayName(displayName);
		result.setName(name);
		if (implicitName != null) {
			result.setImplicitName(implicitName);
			if (name == null) {
				result.setName(implicitName.toString());
			}
		}
		result.setInvisibleConditionName(getInvisibleConditionName());
		result.setRelativeIconFileName(getRelativeIconFileName());
		result.setToolTip(getToolTip());

		return result;
	}
}
