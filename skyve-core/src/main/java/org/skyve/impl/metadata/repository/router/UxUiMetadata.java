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

/**
 * JAXB-annotated descriptor for a named UX/UI variant within the router.
 *
 * <p>A {@code UxUiMetadata} entry maps a symbolic UX/UI name (e.g. {@code desktop},
 * {@code phone}) to an ordered list of {@link Route} instances that override the
 * default routing for that variant.  At runtime the web layer selects the appropriate
 * {@code UxUiMetadata} based on the detected device or session preference and then
 * evaluates its routes before falling back to the global routes in {@link Router}.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see Router
 * @see Route
 * @see org.skyve.metadata.router.UxUi
 */
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
