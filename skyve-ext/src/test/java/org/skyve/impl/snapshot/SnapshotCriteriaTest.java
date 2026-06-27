package org.skyve.impl.snapshot;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SnapshotCriteriaTest {

	@Test
	public void testDefaultOperatorIsAnd() {
		SnapshotCriteria c = new SnapshotCriteria();
		assertEquals(CompoundFilterOperator.and, c.getOperator());
	}

	@Test
	public void testDefaultFiltersIsEmpty() {
		SnapshotCriteria c = new SnapshotCriteria();
		assertNotNull(c.getFilters());
		assertTrue(c.getFilters().isEmpty());
	}

	@Test
	public void testSetOperator() {
		SnapshotCriteria c = new SnapshotCriteria();
		c.setOperator(CompoundFilterOperator.or);
		assertEquals(CompoundFilterOperator.or, c.getOperator());
	}

	@Test
	public void testToMapEmptyFilters() {
		SnapshotCriteria c = new SnapshotCriteria();
		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals(CompoundFilterOperator.and, map.get("operator"));
		assertFalse("no filters key for empty list", map.containsKey("filters"));
	}

	@Test
	public void testToMapWithCriterion() {
		SnapshotCriteria c = new SnapshotCriteria();
		c.setOperator(CompoundFilterOperator.or);

		SnapshotCriterion criterion = new SnapshotCriterion();
		criterion.setColumn("name");
		criterion.setOperator(SmartClientFilterOperator.iContains);
		criterion.setValue("test");
		c.getFilters().add(criterion);

		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals(CompoundFilterOperator.or, map.get("operator"));
		assertTrue("filters key should be present", map.containsKey("filters"));

		@SuppressWarnings("unchecked")
		List<LinkedHashMap<String, Object>> filters = (List<LinkedHashMap<String, Object>>) map.get("filters");
		assertEquals(1, filters.size());
		assertEquals("name", filters.get(0).get("column"));
	}

	@Test
	public void testPopulateFromMapSetsOperator() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "or");

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertEquals(CompoundFilterOperator.or, c.getOperator());
	}

	@Test
	public void testPopulateFromMapWithCriterionFilter() {
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
	public void testPopulateIgnoresNonMapFilterElements() {
		List<Object> filterList = new ArrayList<>();
		filterList.add("notAMap"); // should be ignored

		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");
		map.put("filters", filterList);

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertTrue("non-map elements should be skipped", c.getFilters().isEmpty());
	}

	@Test
	public void testPopulateIgnoresNonListFiltersValue() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");
		map.put("filters", "notAList");

		SnapshotCriteria c = new SnapshotCriteria();
		c.populate(map);

		assertTrue("non-list filters value should be ignored", c.getFilters().isEmpty());
	}
}
