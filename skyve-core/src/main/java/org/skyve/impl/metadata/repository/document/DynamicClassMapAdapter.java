package org.skyve.impl.metadata.repository.document;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * JAXB Adapter for Map<String, String> to/from DynamicClassMapType.
 */
public class DynamicClassMapAdapter extends XmlAdapter<DynamicClassMapType, Map<String, String>> implements Serializable {
	private static final long serialVersionUID = -3958205804507420927L;

	@Override
	public Map<String, String> unmarshal(DynamicClassMapType v) throws Exception {
		Map<String, String> result = new TreeMap<>();
		for (DynamicClassMapEntryType entry : v.classes) {
			result.put(entry.name, entry.className);
		}
		return result;
	}

	@Override
	public DynamicClassMapType marshal(Map<String, String> v) throws Exception {
		DynamicClassMapType result = null;
		if (! v.isEmpty()) {
			result = new DynamicClassMapType();
			List<DynamicClassMapEntryType> properties = result.classes;
			for (Entry<String, String> entry : v.entrySet()) {
				DynamicClassMapEntryType classEntry = new DynamicClassMapEntryType();
				classEntry.name = entry.getKey();
				classEntry.className = entry.getValue();
				properties.add(classEntry);
			}
		}
		
		return result;
	}
}
