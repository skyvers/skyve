package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class VariableExpanderTest {

	private VariableExpander variableExpander;
	private Map<String, String> variables;
	private Map<String, Object> properties;

	@BeforeEach
	public void before() {
		variableExpander = new VariableExpander();
		variables = new HashMap<>();
		properties = new HashMap<>();
	}

	/**
	 * Tests basic config that is only one level deep.
	 */
	@Test
	public void testExpandSingleLevelProperty() {
		final String variableKey = "TEST";
		final String variableValue = "testValue";
		variables.put(variableKey, variableValue);

		final String propertyKey = "testKey";
		final String propertyValue = "${TEST}";
		properties.put(propertyKey, propertyValue);

		final Map<String, Object> expandedConfig = variableExpander.expand(properties, variables);

		final String expandedProperty = (String) expandedConfig.get(propertyKey);
		assertThat(expandedProperty, is(variableValue));
	}

	/**
	 * Tests config that is multi-levels deep (maps within maps).
	 */
	@Test
	@SuppressWarnings("unchecked")
	public void testExpandNestedPropertyMap() {
		final String variableKey = "TEST";
		final String variableValue = "testValue";
		variables.put(variableKey, variableValue);

		final String propertyKey = "testKey";
		final String propertyValue = "${TEST}";
		properties.put(propertyKey, propertyValue);

		final String firstLevelMapPropertyKey = "firstLevelMapKey";
		final Map<String, Object> firstLevelMapPropertyValue = new HashMap<>();
		firstLevelMapPropertyValue.put(propertyKey, propertyValue);

		final String secondLevelMapPropertyKey = "secondLevelMapKey";
		final Map<String, Object> secondLevelMapPropertyValue = new HashMap<>();
		secondLevelMapPropertyValue.put(propertyKey, propertyValue);

		firstLevelMapPropertyValue.put(secondLevelMapPropertyKey, secondLevelMapPropertyValue);
		properties.put(firstLevelMapPropertyKey, firstLevelMapPropertyValue);

		final Map<String, Object> expandedConfig = variableExpander.expand(properties, variables);

		final String expandedProperty = (String) expandedConfig.get(propertyKey);
		assertThat(expandedProperty, is(variableValue));

		final Map<String, Object> expandedFirstLevelPropertyMap =
				(Map<String, Object>) expandedConfig.get(firstLevelMapPropertyKey);
		final String expandedFirstLevelProperty = (String) expandedFirstLevelPropertyMap.get(propertyKey);
		assertThat(expandedFirstLevelProperty, is(variableValue));

		final Map<String, Object> expandedSecondLevelPropertyMap =
				(Map<String, Object>) expandedFirstLevelPropertyMap.get(secondLevelMapPropertyKey);
		final String expandedSecondLevelProperty = (String) expandedSecondLevelPropertyMap.get(propertyKey);
		assertThat(expandedSecondLevelProperty, is(variableValue));
	}

	/**
	 * Tests that the default value is used when the variable is not defined.
	 */
	@Test
	public void testDefaultUsedWhenVariableNotDefined() {
		final String propertyKey = "testKey";
		final String propertyDefaultValue = "defaultValue";
		final String propertyValue = String.format("${TEST:%s}", propertyDefaultValue);
		properties.put(propertyKey, propertyValue);

		final Map<String, Object> expandedConfig = variableExpander.expand(properties, variables);

		final String expandedProperty = (String) expandedConfig.get(propertyKey);
		assertThat(expandedProperty, is(propertyDefaultValue));
	}

	/**
	 * Tests that a quoted string is replaced appropriately.
	 */
	@Test
	public void testExpandQuotedValueWithNullDefault() {
		final String propertyKey = "testKey";
		final String propertyValue = "${TEST:null}";
		properties.put(propertyKey, propertyValue);

		Map<String, Object> expandedConfig = variableExpander.expand(properties, variables);

		String expandedProperty = (String) expandedConfig.get(propertyKey);
		assertThat(expandedProperty, is(nullValue()));

		final String variableKey = "TEST";
		final String variableValue = "\"testValue\"";
		variables.put(variableKey, variableValue);

		expandedConfig = variableExpander.expand(properties, variables);

		expandedProperty = (String) expandedConfig.get(propertyKey);
		assertThat(expandedProperty, is(variableValue));
	}

	@Test
	void testBasicVariableExpansion() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "${NAME}");
		properties.put("age", "${AGE:25}");

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "John");
		variables.put("AGE", "30");

		Map<String, Object> result = variableExpander.expand(properties, variables);

		assertEquals("John", result.get("name"));
		assertEquals("30", result.get("age"));
	}

	@Test
	void testDefaultValueHandling() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "${NAME:Unknown}");
		properties.put("age", "${AGE:25}");

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "John");

		Map<String, Object> result = variableExpander.expand(properties, variables);

		assertEquals("John", result.get("name"));
		assertEquals("25", result.get("age"));
	}

	@Test
	void testNestedMapExpansion() {
		Map<String, Object> properties = new HashMap<>();
		
		Map<String, Object> nestedMap = new HashMap<>();
		nestedMap.put("city", "${CITY}");
		nestedMap.put("country", "${COUNTRY:USA}");
		
		properties.put("address", nestedMap);
		properties.put("name", "${NAME}");

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "John");
		variables.put("CITY", "New York");
		variables.put("COUNTRY", "Canada");

		Map<String, Object> result = variableExpander.expand(properties, variables);

		@SuppressWarnings("unchecked")
		Map<String, Object> expandedAddress = (Map<String, Object>) result.get("address");
		assertEquals("New York", expandedAddress.get("city"));
		assertEquals("Canada", expandedAddress.get("country"));
		assertEquals("John", result.get("name"));
	}

	@Test
	void testNullValueHandling() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "${NAME:null}");
		properties.put("age", "${AGE:25}");

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "null");

		Map<String, Object> result = variableExpander.expand(properties, variables);

		assertNull(result.get("name"));
		assertEquals("25", result.get("age"));
	}

	@Test
	void testCustomDelimiter() {
		VariableExpander expander = new VariableExpander("|");
		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "${NAME|Unknown}");
		properties.put("age", "${AGE|25}");

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "John");

		Map<String, Object> result = expander.expand(properties, variables);

		assertEquals("John", result.get("name"));
		assertEquals("25", result.get("age"));
	}

	@Test
	void testNonStringValues() {
		Map<String, Object> properties = new HashMap<>();
		properties.put("name", "${NAME}");
		properties.put("age", 30);
		properties.put("active", true);

		Map<String, String> variables = new HashMap<>();
		variables.put("NAME", "John");

		Map<String, Object> result = variableExpander.expand(properties, variables);

		assertEquals("John", result.get("name"));
		assertEquals(30, result.get("age"));
		assertEquals(true, result.get("active"));
	}
}
