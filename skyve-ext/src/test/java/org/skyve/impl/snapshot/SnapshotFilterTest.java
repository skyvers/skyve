package org.skyve.impl.snapshot;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SnapshotFilterTest {

	@Test
	void testFromMapReturnsCriterionWhenColumnPresent() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("column", "name");
		map.put("operator", "contains");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);

		assertNotNull(filter);
		assertTrue(filter instanceof SnapshotCriterion, "should be a SnapshotCriterion when column key is present");
	}

	@Test
	void testFromMapReturnsCriteriaWhenNoColumnPresent() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);

		assertNotNull(filter);
		assertTrue(filter instanceof SnapshotCriteria, "should be a SnapshotCriteria when column key is absent");
	}

	@Test
	void testFromMapCriterionPopulatesColumn() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("column", "status");
		map.put("operator", "equals");
		map.put("value", "active");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);
		SnapshotCriterion criterion = (SnapshotCriterion) filter;

		assertEquals("status", criterion.getColumn());
	}

	@Test
	void testFromMapCriteriaPopulatesOperator() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "or");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);
		SnapshotCriteria criteria = (SnapshotCriteria) filter;

		assertEquals(CompoundFilterOperator.or, criteria.getOperator());
	}
}
