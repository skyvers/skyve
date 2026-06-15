package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Decimal2;

/**
 * Tests for {@link InMemoryFilter} covering the pure-Java predicate logic.
 * Uses {@link DynamicBean} as a simple Bean implementation accessible via Binder.
 */
@SuppressWarnings("static-method")
class InMemoryFilterTest {

	private static DynamicBean bean(String binding, Object value) {
		HashMap<String, Object> props = new HashMap<>();
		props.put(binding, value);
		return new DynamicBean("testModule", "testDocument", props);
	}

	private static List<Bean> list(Bean... beans) {
		List<Bean> result = new ArrayList<>();
		for (Bean b : beans) {
			result.add(b);
		}
		return result;
	}

	@Test
	void isEmptyReturnsTrueWhenNoPredicates() {
		InMemoryFilter filter = new InMemoryFilter();
		assertTrue(filter.isEmpty());
	}

	@Test
	void isEmptyReturnsFalseAfterAddingPredicate() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNull("field");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNullFiltersOutNonNullValues() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNull("name");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", null));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotNullFiltersOutNullValues() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotNull("name");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", null));
		filter.filter(rows);
		assertEquals(1, rows.size());
		assertEquals("Alice", rows.get(0).getDynamic("name"));
	}

	@Test
	void addEqualsStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("name", "Alice");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
		assertEquals("Alice", rows.get(0).getDynamic("name"));
	}

	@Test
	void addNotEqualsStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("name", "Bob");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("age", Integer.valueOf(30));
		List<Bean> rows = list(bean("age", Integer.valueOf(30)), bean("age", Integer.valueOf(25)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("age", Integer.valueOf(30));
		List<Bean> rows = list(bean("age", Integer.valueOf(30)), bean("age", Integer.valueOf(25)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(100L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(100L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("10.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("10.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsBooleanFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("active", Boolean.TRUE);
		List<Bean> rows = list(bean("active", Boolean.TRUE), bean("active", Boolean.FALSE));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsBooleanFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("active", Boolean.TRUE);
		List<Bean> rows = list(bean("active", Boolean.TRUE), bean("active", Boolean.FALSE));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsDateFilters() {
		Date date = new Date(0L);
		Date other = new Date(1000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("created", date);
		List<Bean> rows = list(bean("created", date), bean("created", other));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsDateFilters() {
		Date date = new Date(0L);
		Date other = new Date(1000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("created", date);
		List<Bean> rows = list(bean("created", date), bean("created", other));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEqualsIgnoreCaseFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEqualsIgnoreCase("name", "alice");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsIgnoreCaseFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEqualsIgnoreCase("name", "alice");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addContainsFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addContains("name", "li");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotContainsFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotContains("name", "li");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addStartsWithFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addStartsWith("name", "Al");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotStartsWithFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotStartsWith("name", "Al");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addEndsWithFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEndsWith("name", "ce");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEndsWithFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEndsWith("name", "ce");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThan("name", "B");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThan("age", Integer.valueOf(25));
		List<Bean> rows = list(bean("age", Integer.valueOf(20)), bean("age", Integer.valueOf(30)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThan("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(50L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThan("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("5.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanOrEqualToStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThanOrEqualTo("name", "Bob");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addGreaterThanOrEqualToIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThanOrEqualTo("age", Integer.valueOf(25));
		List<Bean> rows = list(bean("age", Integer.valueOf(20)), bean("age", Integer.valueOf(25)), bean("age", Integer.valueOf(30)));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addGreaterThanOrEqualToLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThanOrEqualTo("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(50L)), bean("count", Long.valueOf(100L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addGreaterThanOrEqualToDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThanOrEqualTo("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("5.00")), bean("amount", new Decimal2("10.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addLessThanStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThan("name", "C");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addLessThanIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThan("age", Integer.valueOf(25));
		List<Bean> rows = list(bean("age", Integer.valueOf(20)), bean("age", Integer.valueOf(30)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addLessThanLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThan("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(50L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addLessThanDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThan("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("5.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addLessThanOrEqualToStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThanOrEqualTo("name", "Bob");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addLessThanOrEqualToIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThanOrEqualTo("age", Integer.valueOf(25));
		List<Bean> rows = list(bean("age", Integer.valueOf(20)), bean("age", Integer.valueOf(25)), bean("age", Integer.valueOf(30)));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addLessThanOrEqualToLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThanOrEqualTo("count", Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(50L)), bean("count", Long.valueOf(100L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addLessThanOrEqualToDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThanOrEqualTo("amount", new Decimal2("10.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("5.00")), bean("amount", new Decimal2("10.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addBetweenStringFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addBetween("name", "A", "C");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Dave"));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addBetweenIntegerFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addBetween("age", Integer.valueOf(20), Integer.valueOf(30));
		List<Bean> rows = list(bean("age", Integer.valueOf(15)), bean("age", Integer.valueOf(25)), bean("age", Integer.valueOf(35)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addBetweenLongFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addBetween("count", Long.valueOf(10L), Long.valueOf(100L));
		List<Bean> rows = list(bean("count", Long.valueOf(5L)), bean("count", Long.valueOf(50L)), bean("count", Long.valueOf(200L)));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addBetweenDecimalFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addBetween("amount", new Decimal2("5.00"), new Decimal2("15.00"));
		List<Bean> rows = list(bean("amount", new Decimal2("1.00")), bean("amount", new Decimal2("10.00")), bean("amount", new Decimal2("20.00")));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addInFiltersCorrectly() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addIn("name", "Alice", "Charlie");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addNotInFiltersCorrectly() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotIn("name", "Alice", "Charlie");
		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Charlie"));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addAndCombinesFilters() {
		InMemoryFilter filter1 = new InMemoryFilter();
		filter1.addEquals("name", "Alice");
		InMemoryFilter filter2 = new InMemoryFilter();
		filter2.addEquals("active", Boolean.TRUE);
		filter1.addAnd(filter2);

		HashMap<String, Object> props1 = new HashMap<>();
		props1.put("name", "Alice");
		props1.put("active", Boolean.TRUE);
		DynamicBean beanTrue = new DynamicBean("m", "d", props1);

		HashMap<String, Object> props2 = new HashMap<>();
		props2.put("name", "Alice");
		props2.put("active", Boolean.FALSE);
		DynamicBean beanFalse = new DynamicBean("m", "d", props2);

		List<Bean> rows = list(beanTrue, beanFalse);
		filter1.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addOrCombinesFilters() {
		InMemoryFilter filter1 = new InMemoryFilter();
		filter1.addEquals("name", "Alice");
		InMemoryFilter filter2 = new InMemoryFilter();
		filter2.addEquals("name", "Bob");
		filter1.addOr(filter2);

		List<Bean> rows = list(bean("name", "Alice"), bean("name", "Bob"), bean("name", "Charlie"));
		filter1.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addGreaterThanDateFilters() {
		Date earlier = new Date(0L);
		Date later = new Date(10000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThan("created", earlier);
		List<Bean> rows = list(bean("created", earlier), bean("created", later));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addGreaterThanOrEqualToDateFilters() {
		Date d1 = new Date(0L);
		Date d2 = new Date(5000L);
		Date d3 = new Date(10000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addGreaterThanOrEqualTo("created", d2);
		List<Bean> rows = list(bean("created", d1), bean("created", d2), bean("created", d3));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addLessThanDateFilters() {
		Date earlier = new Date(0L);
		Date later = new Date(10000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThan("created", later);
		List<Bean> rows = list(bean("created", earlier), bean("created", later));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addLessThanOrEqualToDateFilters() {
		Date d1 = new Date(0L);
		Date d2 = new Date(5000L);
		Date d3 = new Date(10000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addLessThanOrEqualTo("created", d2);
		List<Bean> rows = list(bean("created", d1), bean("created", d2), bean("created", d3));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	@Test
	void addBetweenDateFilters() {
		Date d1 = new Date(0L);
		Date d2 = new Date(5000L);
		Date d3 = new Date(10000L);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addBetween("created", d1, d2);
		List<Bean> rows = list(bean("created", d1), bean("created", d2), bean("created", d3));
		filter.filter(rows);
		assertEquals(2, rows.size());
	}

	// ---- Enum predicates ----

	enum TestStatus { ACTIVE, INACTIVE }

	@Test
	void addEqualsEnumFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("status", TestStatus.ACTIVE);
		List<Bean> rows = list(bean("status", TestStatus.ACTIVE), bean("status", TestStatus.INACTIVE));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsEnumFilters() {
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("status", TestStatus.ACTIVE);
		List<Bean> rows = list(bean("status", TestStatus.ACTIVE), bean("status", TestStatus.INACTIVE));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	// ---- Geometry predicates ----

	private static final GeometryFactory GF = new GeometryFactory();

	private static Geometry box(double x1, double y1, double x2, double y2) {
		return GF.createPolygon(new Coordinate[] {
			new Coordinate(x1, y1), new Coordinate(x2, y1),
			new Coordinate(x2, y2), new Coordinate(x1, y2), new Coordinate(x1, y1)
		});
	}

	@Test
	void addEqualsGeometryFiltersRows() {
		Geometry g1 = box(0, 0, 5, 5);
		Geometry g2 = box(0, 0, 5, 5); // topologically equal
		Geometry g3 = box(10, 10, 15, 15);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("geom", g1);
		List<Bean> rows = list(bean("geom", g2), bean("geom", g3));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addNotEqualsGeometryFiltersRows() {
		Geometry g1 = box(0, 0, 5, 5);
		Geometry g2 = box(0, 0, 5, 5);
		Geometry g3 = box(10, 10, 15, 15);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addNotEquals("geom", g1);
		List<Bean> rows = list(bean("geom", g2), bean("geom", g3));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addWithinFiltersGeometryRows() {
		Geometry bigBox = box(0, 0, 10, 10);
		Geometry smallBox = box(1, 1, 5, 5);   // within bigBox
		Geometry outsideBox = box(20, 20, 30, 30);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addWithin("geom", bigBox);
		List<Bean> rows = list(bean("geom", smallBox), bean("geom", outsideBox));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addContainsGeometryFiltersRows() {
		Geometry bigBox = box(0, 0, 10, 10);
		Geometry smallBox = box(1, 1, 5, 5);
		Geometry outsideBox = box(20, 20, 30, 30);
		InMemoryFilter filter = new InMemoryFilter();
		filter.addContains("geom", smallBox);
		List<Bean> rows = list(bean("geom", bigBox), bean("geom", outsideBox));
		filter.filter(rows);
		assertEquals(1, rows.size()); // bigBox contains smallBox
	}

	@Test
	void addDisjointFiltersGeometryRows() {
		Geometry box1 = box(0, 0, 5, 5);
		Geometry box2 = box(10, 10, 15, 15); // disjoint from box1
		Geometry box3 = box(1, 1, 3, 3);    // within box1, not disjoint
		InMemoryFilter filter = new InMemoryFilter();
		filter.addDisjoint("geom", box1);
		List<Bean> rows = list(bean("geom", box2), bean("geom", box3));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addIntersectsFiltersGeometryRows() {
		Geometry box1 = box(0, 0, 5, 5);
		Geometry box2 = box(3, 3, 8, 8); // overlaps/intersects box1
		Geometry box3 = box(10, 10, 15, 15); // disjoint, does not intersect
		InMemoryFilter filter = new InMemoryFilter();
		filter.addIntersects("geom", box1);
		List<Bean> rows = list(bean("geom", box2), bean("geom", box3));
		filter.filter(rows);
		assertEquals(1, rows.size());
	}

	@Test
	void addCrossesFiltersGeometryRows() {
		// Two linestrings that cross at a point
		Geometry line1 = GF.createLineString(new Coordinate[] { new Coordinate(0, 0), new Coordinate(4, 4) });
		Geometry line2 = GF.createLineString(new Coordinate[] { new Coordinate(0, 4), new Coordinate(4, 0) });
		Geometry line3 = GF.createLineString(new Coordinate[] { new Coordinate(10, 10), new Coordinate(20, 20) });
		InMemoryFilter filter = new InMemoryFilter();
		filter.addCrosses("geom", line1);
		List<Bean> rows = list(bean("geom", line2), bean("geom", line3));
		filter.filter(rows);
		assertEquals(1, rows.size()); // line2 crosses line1
	}

	@Test
	void addOverlapsFiltersGeometryRows() {
		Geometry box1 = box(0, 0, 4, 4);
		Geometry box2 = box(2, 2, 6, 6); // overlaps with box1
		Geometry box3 = box(10, 10, 15, 15); // disjoint
		InMemoryFilter filter = new InMemoryFilter();
		filter.addOverlaps("geom", box1);
		List<Bean> rows = list(bean("geom", box2), bean("geom", box3));
		filter.filter(rows);
		assertEquals(1, rows.size()); // box2 overlaps box1
	}

	@Test
	void addTouchesFiltersGeometryRows() {
		Geometry box1 = box(0, 0, 3, 3);
		Geometry box2 = box(3, 0, 6, 3); // shares edge x=3 with box1 — touches
		Geometry box3 = box(10, 10, 15, 15); // disjoint
		InMemoryFilter filter = new InMemoryFilter();
		filter.addTouches("geom", box1);
		List<Bean> rows = list(bean("geom", box2), bean("geom", box3));
		filter.filter(rows);
		assertEquals(1, rows.size()); // box2 touches box1
	}

	@Test
	void evaluateExceptionCausesBeanToBeExcluded() {
		// Passing a null bean to filter() causes Binder.get(null, binding) to throw
		// which hits the catch block in MyPredicate.evaluate(Object) → returns false → item removed
		InMemoryFilter filter = new InMemoryFilter();
		filter.addEquals("field", "value");
		List<Bean> rows = new ArrayList<>();
		rows.add(null); // null bean will throw in evaluate
		filter.filter(rows);
		assertTrue(rows.isEmpty(), "Null bean should be excluded due to exception in evaluate");
	}
}

