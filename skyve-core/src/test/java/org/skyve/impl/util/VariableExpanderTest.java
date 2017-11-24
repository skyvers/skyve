package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class VariableExpanderTest {

	private VariableExpander variableExpander;
	private Map<String, String> variables;
	private Map<String, Object> properties;

	@Before
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
		Assert.assertThat(expandedProperty, is(variableValue));
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
		Assert.assertThat(expandedProperty, is(variableValue));

		final Map<String, Object> expandedFirstLevelPropertyMap =
				(Map<String, Object>) expandedConfig.get(firstLevelMapPropertyKey);
		final String expandedFirstLevelProperty = (String) expandedFirstLevelPropertyMap.get(propertyKey);
		Assert.assertThat(expandedFirstLevelProperty, is(variableValue));

		final Map<String, Object> expandedSecondLevelPropertyMap =
				(Map<String, Object>) expandedFirstLevelPropertyMap.get(secondLevelMapPropertyKey);
		final String expandedSecondLevelProperty = (String) expandedSecondLevelPropertyMap.get(propertyKey);
		Assert.assertThat(expandedSecondLevelProperty, is(variableValue));
;	}

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
		Assert.assertThat(expandedProperty, is(propertyDefaultValue));
	}
}
