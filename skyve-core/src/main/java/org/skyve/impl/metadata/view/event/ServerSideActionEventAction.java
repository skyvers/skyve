package org.skyve.impl.metadata.view.event;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "server")
public class ServerSideActionEventAction implements EventAction {
	private static final long serialVersionUID = -7029807092282914910L;

	private String actionName;
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	public String getActionName() {
		return actionName;
	}
	
	@XmlAttribute(name = "action", required = true)
	public void setActionName(String actionName) {
		this.actionName = actionName;
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
