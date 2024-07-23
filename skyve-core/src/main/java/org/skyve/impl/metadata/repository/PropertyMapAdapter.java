package org.skyve.impl.metadata.repository;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

/**
 * JAXB Adapter for Map<String, String> to/from PropertyMapType.
 */
public class PropertyMapAdapter extends XmlAdapter<PropertyMapType, Map<String, String>> implements Serializable {
	private static final long serialVersionUID = 729761523390093996L;

	@Override
	public Map<String, String> unmarshal(PropertyMapType v) throws Exception {
		Map<String, String> result = new TreeMap<>();
		for (PropertyMapEntryType property : v.properties) {
			result.put(property.key, property.value);
		}
		return result;
	}

	@Override
	public PropertyMapType marshal(Map<String, String> v) throws Exception {
		PropertyMapType result = null;
		if (! v.isEmpty()) {
			result = new PropertyMapType();
			List<PropertyMapEntryType> properties = result.properties;
			for (Entry<String, String> entry : v.entrySet()) {
				PropertyMapEntryType property = new PropertyMapEntryType();
				property.key = entry.getKey();
				property.value = entry.getValue();
				properties.add(property);
			}
		}
		
		return result;
	}
}
