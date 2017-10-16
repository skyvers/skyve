package org.skyve.impl.metadata.repository.view.actions;

import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			name = "abstractAction",
			propOrder = {"name", 
							"displayName",
							"toolTip", 
							"relativeIconFileName", 
							"iconStyleClass", 
							"confirmationText",
							"disabledConditionName",
							"enabledConditionName",
							"invisibleConditionName",
							"visibleConditionName",
							"properties"})
public abstract class ActionMetaData {
	protected ImplicitActionName implicitName;

	private String name;
	private String displayName;
	private String toolTip;
	private String relativeIconFileName;
	private String iconStyleClass;
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

	public String getIconStyleClass() {
		return iconStyleClass;
	}

	@XmlAttribute(required = false)
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
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

	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getVisibleConditionName() {
		return null;
	}

	@XmlAttribute(name = "visible", required = false)
	public void setVisibleConditionName(String visibleConditionName) {
		this.invisibleConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(visibleConditionName));
	}

	public String getDisabledConditionName() {
		return disabledConditionName;
	}

	@XmlAttribute(name = "disabled", required = false)
	public void setDisabledConditionName(String disabledConditionName) {
		this.disabledConditionName = UtilImpl.processStringValue(disabledConditionName);
	}
	
	// to enable JAXB XML marshaling
	@SuppressWarnings("static-method")
	String getEnabledConditionName() {
		return null;
	}

	@XmlAttribute(name = "enabled", required = false)
	public void setEnabledConditionName(String enabledConditionName) {
		this.disabledConditionName = BindUtil.negateCondition(UtilImpl.processStringValue(enabledConditionName));
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	public Map<String, String> properties = new TreeMap<>();

	public Map<String, String> getProperties() {
		return properties;
	}

	public ActionImpl toMetaDataAction() {
		ActionImpl result = new ActionImpl();

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
		result.setIconStyleClass(getIconStyleClass());
		result.setToolTip(getToolTip());
		result.setProperties(properties);

		return result;
	}
}
