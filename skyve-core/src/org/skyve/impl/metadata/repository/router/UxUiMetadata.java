package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class UxUiMetadata extends NamedMetaData {
	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE, name = "route", required = true)
	private List<Route> routes = new ArrayList<>();

	public List<Route> getRoutes() {
		return routes;
	}
}
