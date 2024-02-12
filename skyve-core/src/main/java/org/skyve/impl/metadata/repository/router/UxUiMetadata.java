package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class UxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = 7649740185688983705L;

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "route", required = true)
	private List<Route> routes = new ArrayList<>();

	public List<Route> getRoutes() {
		return routes;
	}
}
