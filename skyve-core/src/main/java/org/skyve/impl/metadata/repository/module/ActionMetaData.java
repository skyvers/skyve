package org.skyve.impl.metadata.repository.module;

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

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ActionMetaData extends NamedMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = 4928567931483027768L;

	private List<ApplicableTo> uxuis = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "uxui", required = false)
	public List<ApplicableTo> getUxuis() {
		return uxuis;
	}
	
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
