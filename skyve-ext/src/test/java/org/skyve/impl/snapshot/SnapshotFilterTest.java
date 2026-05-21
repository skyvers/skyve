package org.skyve.impl.snapshot;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SnapshotFilterTest {

	@Test
	public void testFromMapReturnsCriterionWhenColumnPresent() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("column", "name");
		map.put("operator", "contains");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);

		assertNotNull(filter);
		assertTrue("should be a SnapshotCriterion when column key is present", filter instanceof SnapshotCriterion);
	}

	@Test
	public void testFromMapReturnsCriteriaWhenNoColumnPresent() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "and");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);

		assertNotNull(filter);
		assertTrue("should be a SnapshotCriteria when column key is absent", filter instanceof SnapshotCriteria);
	}

	@Test
	public void testFromMapCriterionPopulatesColumn() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("column", "status");
		map.put("operator", "equals");
		map.put("value", "active");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);
		SnapshotCriterion criterion = (SnapshotCriterion) filter;

		assertEquals("status", criterion.getColumn());
	}

	@Test
	public void testFromMapCriteriaPopulatesOperator() {
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("operator", "or");

		SnapshotFilter filter = SnapshotFilter.fromMap(map);
		SnapshotCriteria criteria = (SnapshotCriteria) filter;

		assertEquals(CompoundFilterOperator.or, criteria.getOperator());
	}
}
