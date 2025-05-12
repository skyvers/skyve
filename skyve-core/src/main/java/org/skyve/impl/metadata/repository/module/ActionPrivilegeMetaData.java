package org.skyve.impl.metadata.repository.module;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ActionPrivilegeMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 1294697285074132505L;

	private String actionName;

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();
	
	public String getActionName() {
		return actionName;
	}

	@XmlAttribute(name = "name", required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
