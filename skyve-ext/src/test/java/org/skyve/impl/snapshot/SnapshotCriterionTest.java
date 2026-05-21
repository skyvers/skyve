package org.skyve.impl.snapshot;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.LinkedHashMap;

import org.junit.Test;

@SuppressWarnings("static-method")
public class SnapshotCriterionTest {

	@Test
	public void testDefaultStateIsNull() {
		SnapshotCriterion c = new SnapshotCriterion();
		assertNull(c.getColumn());
		assertNull(c.getOperator());
		assertNull(c.getValue());
		assertNull(c.getStart());
		assertNull(c.getEnd());
	}

	@Test
	public void testSetAndGetColumn() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("myColumn");
		assertEquals("myColumn", c.getColumn());
	}

	@Test
	public void testSetAndGetOperator() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setOperator(SmartClientFilterOperator.equals);
		assertEquals(SmartClientFilterOperator.equals, c.getOperator());
	}

	@Test
	public void testSetAndGetValue() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setValue("hello");
		assertEquals("hello", c.getValue());
	}

	@Test
	public void testSetAndGetStart() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setStart("2024-01-01");
		assertEquals("2024-01-01", c.getStart());
	}

	@Test
	public void testSetAndGetEnd() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setEnd("2024-12-31");
		assertEquals("2024-12-31", c.getEnd());
	}

	@Test
	public void testToMapWithValueOnly() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("name");
		c.setOperator(SmartClientFilterOperator.contains);
		c.setValue("Smith");

		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals("name", map.get("column"));
		assertEquals(SmartClientFilterOperator.contains, map.get("operator"));
		assertEquals("Smith", map.get("value"));
		assertNull(map.get("start"));
		assertNull(map.get("end"));
	}

	@Test
	public void testToMapWithStartAndEnd() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("date");
		c.setOperator(SmartClientFilterOperator.greaterThan);
		c.setStart("2024-01-01");
		c.setEnd("2024-12-31");

		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals("date", map.get("column"));
		assertEquals("2024-01-01", map.get("start"));
		assertEquals("2024-12-31", map.get("end"));
		assertNull(map.get("value"));
	}

	@Test
	public void testToMapWithNullValueStartEnd() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("status");
		c.setOperator(SmartClientFilterOperator.notEqual);

		LinkedHashMap<String, Object> map = c.toMap();
		assertEquals("status", map.get("column"));
		assertNull(map.get("value"));
		assertNull(map.get("start"));
		assertNull(map.get("end"));
	}

	@Test
	public void testToMapStartWithoutEndExcludesRange() {
		SnapshotCriterion c = new SnapshotCriterion();
		c.setColumn("col");
		c.setOperator(SmartClientFilterOperator.lessThan);
		c.setStart("100");
		// end is still null

		LinkedHashMap<String, Object> map = c.toMap();
		// Both start AND end must be non-null for range to appear
		assertNull(map.get("start"));
		assertNull(map.get("end"));
	}

	@Test
	public void testPopulateFromMap() {
		java.util.LinkedHashMap<String, Object> map = new java.util.LinkedHashMap<>();
		map.put("column", "age");
		map.put("operator", "greaterOrEqual");
		map.put("value", Integer.valueOf(18));

		SnapshotCriterion c = new SnapshotCriterion();
		c.populate(map);

		assertEquals("age", c.getColumn());
		assertEquals(SmartClientFilterOperator.greaterOrEqual, c.getOperator());
		assertEquals(Integer.valueOf(18), c.getValue());
	}

	@Test
	public void testPopulateFromMapWithStartAndEnd() {
		java.util.LinkedHashMap<String, Object> map = new java.util.LinkedHashMap<>();
		map.put("column", "score");
		map.put("operator", "iBetween");
		map.put("start", Integer.valueOf(10));
		map.put("end", Integer.valueOf(100));

		SnapshotCriterion c = new SnapshotCriterion();
		c.populate(map);

		assertEquals("score", c.getColumn());
		assertEquals(Integer.valueOf(10), c.getStart());
		assertEquals(Integer.valueOf(100), c.getEnd());
	}

	@Test
	public void testPopulateIgnoresMissingKeys() {
		java.util.LinkedHashMap<String, Object> map = new java.util.LinkedHashMap<>();
		map.put("column", "active");

		SnapshotCriterion c = new SnapshotCriterion();
		c.populate(map);

		assertEquals("active", c.getColumn());
		assertNull(c.getOperator());
		assertNull(c.getValue());
	}
}
