package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ActionPrivilege {
	private String actionName;

	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}
}
