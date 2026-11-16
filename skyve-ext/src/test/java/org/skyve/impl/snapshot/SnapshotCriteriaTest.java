package org.skyve.impl.snapshot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SnapshotCriteriaTest {

	@Test
	void testDefaultOperatorIsAnd() {
		SnapshotCriteria c = new SnapshotCriteria();
		assertEquals(CompoundFilterOperator.and, c.getOperator());
	}

	@Test
	void testDefaultFiltersIsEmpty() {
		SnapshotCriteria c = new SnapshotCriteria();
		assertNotNull(c.getFilters());
		assertTrue(c.getFilters().isEmpty());
	}

	@Test
	void testSetOperator() {
		SnapshotCriteria c = new SnapshotCriteria();
		c.setOperator(CompoundFilterOperator.or);
		assertEquals(CompoundFilterOperator.or, c.getOperator());
	}

	@Test
	void testToMapEmptyFilters() {
		SnapshotCriteria c = new SnapshotCriteria();
		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals(CompoundFilterOperator.and, map.get("operator"));
		assertFalse(map.containsKey("filters"), "no filters key for empty list");
	}

	@Test
	void testToMapWithCriterion() {
		SnapshotCriteria c = new SnapshotCriteria();
		c.setOperator(CompoundFilterOperator.or);

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("name");
		criterion.setOperator(SmartClientFilterOperator.iContains);
		criterion.setValue("test");
		c.getFilters().add(criterion);

		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals(CompoundFilterOperator.or, map.get("operator"));
		assertTrue(map.containsKey("filters"), "filters key should be present");

		@SuppressWarnings("unchecked")
		List<LinkedHashMap<String, Object>> filters = (List<LinkedHashMap<String, Object>>) map.get("filters");
		assertEquals(1, filters.size());
		assertEquals("name", filters.get(0).get("column"));
	}

	@Test
	void testPopulateFromMapSetsOperator() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "or");

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertEquals(CompoundFilterOperator.or, c.getOperator());
	}

	@Test
	void testPopulateFromMapWithCriterionFilter() {
		// Build a criterion-shaped inner map
		Map<String, Object> criterionMap = new LinkedHashMap<>();
		criterionMap.put("column", "status");
		criterionMap.put("operator", "equals");
		criterionMap.put("value", "active");

		List<Object> filterList = new ArrayList<>();
		filterList.add(criterionMap);

		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");
		map.put("filters", filterList);

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertEquals(CompoundFilterOperator.and, c.getOperator());
		assertEquals(1, c.getFilters().size());
		assertTrue(c.getFilters().get(0) instanceof SnapshotCriterion);

		SnapshotCriterion inner = (SnapshotCriterion) c.getFilters().get(0);
		assertEquals("status", inner.getColumn());
	}

	@Test
	void testPopulateIgnoresNonMapFilterElements() {
		List<Object> filterList = new ArrayList<>();
		filterList.add("notAMap"); // should be ignored

		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");
		map.put("filters", filterList);

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertTrue(c.getFilters().isEmpty(), "non-map elements should be skipped");
	}

	@Test
	void testPopulateIgnoresNonListFiltersValue() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");
		map.put("filters", "notAList");

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertTrue(c.getFilters().isEmpty(), "non-list filters value should be ignored");
	}
}
