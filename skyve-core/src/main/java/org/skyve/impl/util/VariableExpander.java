package org.skyve.impl.util;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.text.StringSubstitutor;

/**
 * Used to replace Strings in a property map with a value defined by a variable.
 */
public class VariableExpander {
	final static String DEFAULT_DELIMITER = ":";

	private final String delimiter;

	public VariableExpander() {
		this(DEFAULT_DELIMITER);
	}

	public VariableExpander(String delimiter) {
		this.delimiter = delimiter;
	}

	/**
	 * Replaces any variables referenced in string properties with variables defined in the given map.
	 *
	 * Format: ${VARIABLE_NAME:defaultValue}
	 *
	 */
	@SuppressWarnings("unchecked")
	public Map<String, Object> expand(Map<String, Object> properties, Map<String, String> variables) {
		final Map<String, Object> expandedProperties = new HashMap<>();
		final StringSubstitutor stringSubstitutor = new StringSubstitutor(variables); 
		stringSubstitutor.setValueDelimiter(delimiter);

		for (Map.Entry<String, Object> property : properties.entrySet()) {
			final String key = property.getKey();
			Object value = property.getValue();
			if (value instanceof String) {
				value = stringSubstitutor.replace(value);
				if (Objects.equals(value, "null")) {
					value = null;
				}
			} else if (value instanceof Map) {
				value = expand((Map<String, Object>) value, variables);
			}

			expandedProperties.put(key, value);
		}

		return expandedProperties;
	}
}
