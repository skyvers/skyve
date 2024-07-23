package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ActionReference implements Reference {
	private static final long serialVersionUID = -3467116310017720893L;

	private String actionName;

	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(name = "action", required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}
}
