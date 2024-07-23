package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlType(namespace = XMLMetaData.ROUTER_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
public class Route implements DecoratedMetaData {
	private static final long serialVersionUID = 2672586575464268486L;

	private String outcomeUrl;

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
	private List<RouteCriteria> criteria = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.ROUTER_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	public String getOutcomeUrl() {
		return outcomeUrl;
	}
	@XmlAttribute(name = "outcome", required = true)
	public void setOutcomeUrl(String outcomeUrl) {
		this.outcomeUrl = UtilImpl.processStringValue(outcomeUrl);
	}
	
	public List<RouteCriteria> getCriteria() {
		return criteria;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
