package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE, propOrder = {"routes", "properties"})
public class UxUiMetadata extends NamedMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 7649740185688983705L;

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "route", required = true)
	private List<Route> routes = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	public List<Route> getRoutes() {
		return routes;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
