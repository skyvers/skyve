package org.skyve.impl.metadata.repository.document;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

/**
 * JAXB Adapter for Map<String, String> to/from DynamicClassMapType.
 *
 * <p>The XML representation stores repeated {@code <class>} elements while the
 * runtime representation expects a {@code Map<String, String>} keyed by logical
 * name.
 */
public class DynamicClassMapAdapter extends XmlAdapter<DynamicClassMapType, Map<String, String>> implements Serializable {
	private static final long serialVersionUID = -3958205804507420927L;

	/**
	 * Converts JAXB list representation into a sorted runtime map.
	 *
	 * @param v JAXB wrapper containing dynamic class entries
	 * @return a map keyed by entry name with class names as values
	 */
	@Override
	public Map<String, String> unmarshal(DynamicClassMapType v) throws Exception {
		Map<String, String> result = new TreeMap<>();
		for (DynamicClassMapEntryType entry : v.classes) {
			result.put(entry.name, entry.className);
		}
		return result;
	}

	/**
	 * Converts runtime map representation into JAXB list form.
	 *
	 * @param v dynamic class mappings keyed by logical name
	 * @return JAXB wrapper, or {@code null} when the map is empty
	 */
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
