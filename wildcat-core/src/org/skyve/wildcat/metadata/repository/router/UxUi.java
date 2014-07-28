package org.skyve.wildcat.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.ROUTER_NAMESPACE)
public class UxUi extends NamedMetaData {
	@XmlElement(namespace = XMLUtil.ROUTER_NAMESPACE, name = "route", required = true)
	private List<Route> routes = new ArrayList<>();

	public List<Route> getRoutes() {
		return routes;
	}
}
