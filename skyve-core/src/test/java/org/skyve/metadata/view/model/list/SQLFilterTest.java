package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;
import org.skyve.persistence.SQL;

@SuppressWarnings("java:S8692") // system clock OK
class SQLFilterTest {

	private SQLFilter filter;
	private SQLFilter filterNoSummary;

	@BeforeEach
	void setUp() {
		User user = mock(User.class);
		AbstractPersistence persistence =
			mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		filter = new SQLFilter(mock(SQL.class), mock(SQL.class));
		filterNoSummary = new SQLFilter(mock(SQL.class), null);
	}

	@AfterEach
	@SuppressWarnings({"static-method", "unchecked"})
	void tearDown() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	// --- isEmpty ---

	@Test
	void newFilterIsEmpty() {
		assertTrue(filter.isEmpty());
	}

	// --- addAnd ---

	@Test
	void addAndSetsNotEmpty() {
		filter.addAnd(new SQLFilter(mock(SQL.class), mock(SQL.class)));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addAndNoSummarySetsNotEmpty() {
		filterNoSummary.addAnd(new SQLFilter(mock(SQL.class), null));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addOr ---

	@Test
	void addOrSetsNotEmpty() {
		filter.addOr(new SQLFilter(mock(SQL.class), mock(SQL.class)));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOrNoSummarySetsNotEmpty() {
		filterNoSummary.addOr(new SQLFilter(mock(SQL.class), null));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addTagged ---

	@Test
	void addTaggedTrueSetsNotEmpty() {
		filter.addTagged("tag-1", true);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTaggedFalseSetsNotEmpty() {
		filter.addTagged("tag-1", false);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTaggedNoSummarySetsNotEmpty() {
		filterNoSummary.addTagged("tag-1", true);
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addNull ---

	@Test
	void addNullSetsNotEmpty() {
		filter.addNull("name");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNullNoSummarySetsNotEmpty() {
		filterNoSummary.addNull("name");
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addNotNull ---

	@Test
	void addNotNullSetsNotEmpty() {
		filter.addNotNull("name");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotNullNoSummarySetsNotEmpty() {
		filterNoSummary.addNotNull("name");
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, String) ---

	@Test
	void addEqualsStringSetsNotEmpty() {
		filter.addEquals("name", "value");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsStringNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("name", "value");
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Date) ---

	@Test
	void addEqualsDateSetsNotEmpty() {
		filter.addEquals("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDateNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Integer) ---

	@Test
	void addEqualsIntegerSetsNotEmpty() {
		filter.addEquals("count", Integer.valueOf(5));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("count", Integer.valueOf(5));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Long) ---

	@Test
	void addEqualsLongSetsNotEmpty() {
		filter.addEquals("count", Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsLongNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("count", Long.valueOf(100L));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Decimal) ---

	@Test
	void addEqualsDecimalSetsNotEmpty() {
		filter.addEquals("amount", new Decimal2("1.23"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("amount", new Decimal2("1.23"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Boolean) ---

	@Test
	void addEqualsBooleanSetsNotEmpty() {
		filter.addEquals("flag", Boolean.TRUE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsBooleanNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("flag", Boolean.TRUE);
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Enum<?>) ---

	@Test
	void addEqualsEnumSetsNotEmpty() {
		filter.addEquals("status", Thread.State.RUNNABLE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsEnumNoSummarySetsNotEmpty() {
		filterNoSummary.addEquals("status", Thread.State.RUNNABLE);
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addEquals(String, Geometry) ---

	@Test
	void addEqualsGeometrySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addEquals("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsGeometryNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addEquals("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addNotEquals ---

	@Test
	void addNotEqualsStringSetsNotEmpty() {
		filter.addNotEquals("name", "other");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsStringNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("name", "other");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsDateSetsNotEmpty() {
		filter.addNotEquals("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDateNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsIntegerSetsNotEmpty() {
		filter.addNotEquals("count", Integer.valueOf(5));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("count", Integer.valueOf(5));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsLongSetsNotEmpty() {
		filter.addNotEquals("count", Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsLongNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("count", Long.valueOf(100L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsDecimalSetsNotEmpty() {
		filter.addNotEquals("amount", new Decimal2("1.23"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("amount", new Decimal2("1.23"));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsBooleanSetsNotEmpty() {
		filter.addNotEquals("flag", Boolean.FALSE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsBooleanNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("flag", Boolean.FALSE);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsEnumSetsNotEmpty() {
		filter.addNotEquals("status", Thread.State.WAITING);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsEnumNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEquals("status", Thread.State.WAITING);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsGeometrySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(3.0, 4.0));
		filter.addNotEquals("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsGeometryNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(3.0, 4.0));
		filterNoSummary.addNotEquals("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- case-insensitive ---

	@Test
	void addEqualsIgnoreCaseSetsNotEmpty() {
		filter.addEqualsIgnoreCase("name", "VALUE");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsIgnoreCaseNoSummarySetsNotEmpty() {
		filterNoSummary.addEqualsIgnoreCase("name", "VALUE");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEqualsIgnoreCaseSetsNotEmpty() {
		filter.addNotEqualsIgnoreCase("name", "VALUE");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIgnoreCaseNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEqualsIgnoreCase("name", "VALUE");
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- contains / startsWith / endsWith ---

	@Test
	void addContainsStringSetsNotEmpty() {
		filter.addContains("name", "foo");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addContainsStringNoSummarySetsNotEmpty() {
		filterNoSummary.addContains("name", "foo");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotContainsStringSetsNotEmpty() {
		filter.addNotContains("name", "foo");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotContainsStringNoSummarySetsNotEmpty() {
		filterNoSummary.addNotContains("name", "foo");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addStartsWithSetsNotEmpty() {
		filter.addStartsWith("name", "foo");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addStartsWithNoSummarySetsNotEmpty() {
		filterNoSummary.addStartsWith("name", "foo");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotStartsWithSetsNotEmpty() {
		filter.addNotStartsWith("name", "foo");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotStartsWithNoSummarySetsNotEmpty() {
		filterNoSummary.addNotStartsWith("name", "foo");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addEndsWithSetsNotEmpty() {
		filter.addEndsWith("name", "bar");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEndsWithNoSummarySetsNotEmpty() {
		filterNoSummary.addEndsWith("name", "bar");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotEndsWithSetsNotEmpty() {
		filter.addNotEndsWith("name", "bar");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEndsWithNoSummarySetsNotEmpty() {
		filterNoSummary.addNotEndsWith("name", "bar");
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- greaterThan ---

	@Test
	void addGreaterThanStringSetsNotEmpty() {
		filter.addGreaterThan("name", "a");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanStringNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThan("name", "a");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanDateSetsNotEmpty() {
		filter.addGreaterThan("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDateNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThan("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanIntegerSetsNotEmpty() {
		filter.addGreaterThan("count", Integer.valueOf(0));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThan("count", Integer.valueOf(0));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanLongSetsNotEmpty() {
		filter.addGreaterThan("count", Long.valueOf(0L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanLongNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThan("count", Long.valueOf(0L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanDecimalSetsNotEmpty() {
		filter.addGreaterThan("amount", new Decimal2("0.01"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThan("amount", new Decimal2("0.01"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- greaterThanOrEqualTo ---

	@Test
	void addGreaterThanOrEqualToStringSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("name", "a");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToStringNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThanOrEqualTo("name", "a");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDateSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDateNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThanOrEqualTo("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToIntegerSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("count", Integer.valueOf(1));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThanOrEqualTo("count", Integer.valueOf(1));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToLongSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("count", Long.valueOf(1L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToLongNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThanOrEqualTo("count", Long.valueOf(1L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDecimalSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("amount", new Decimal2("1.00"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addGreaterThanOrEqualTo("amount", new Decimal2("1.00"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- lessThan ---

	@Test
	void addLessThanStringSetsNotEmpty() {
		filter.addLessThan("name", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanStringNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThan("name", "z");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanDateSetsNotEmpty() {
		filter.addLessThan("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDateNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThan("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanIntegerSetsNotEmpty() {
		filter.addLessThan("count", Integer.valueOf(99));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThan("count", Integer.valueOf(99));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanLongSetsNotEmpty() {
		filter.addLessThan("count", Long.valueOf(99L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanLongNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThan("count", Long.valueOf(99L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanDecimalSetsNotEmpty() {
		filter.addLessThan("amount", new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThan("amount", new Decimal2("9.99"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- lessThanOrEqualTo ---

	@Test
	void addLessThanOrEqualToStringSetsNotEmpty() {
		filter.addLessThanOrEqualTo("name", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToStringNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThanOrEqualTo("name", "z");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDateSetsNotEmpty() {
		filter.addLessThanOrEqualTo("date", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDateNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThanOrEqualTo("date", new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanOrEqualToIntegerSetsNotEmpty() {
		filter.addLessThanOrEqualTo("count", Integer.valueOf(10));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThanOrEqualTo("count", Integer.valueOf(10));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanOrEqualToLongSetsNotEmpty() {
		filter.addLessThanOrEqualTo("count", Long.valueOf(10L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToLongNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThanOrEqualTo("count", Long.valueOf(10L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDecimalSetsNotEmpty() {
		filter.addLessThanOrEqualTo("amount", new Decimal2("5.00"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addLessThanOrEqualTo("amount", new Decimal2("5.00"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addBetween ---

	@Test
	void addBetweenStringSetsNotEmpty() {
		filter.addBetween("name", "a", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenStringNoSummarySetsNotEmpty() {
		filterNoSummary.addBetween("name", "a", "z");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addBetweenDateSetsNotEmpty() {
		filter.addBetween("date", new Date(0), new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDateNoSummarySetsNotEmpty() {
		filterNoSummary.addBetween("date", new Date(0), new Date());
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addBetweenIntegerSetsNotEmpty() {
		filter.addBetween("count", Integer.valueOf(1), Integer.valueOf(10));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenIntegerNoSummarySetsNotEmpty() {
		filterNoSummary.addBetween("count", Integer.valueOf(1), Integer.valueOf(10));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addBetweenLongSetsNotEmpty() {
		filter.addBetween("count", Long.valueOf(1L), Long.valueOf(10L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenLongNoSummarySetsNotEmpty() {
		filterNoSummary.addBetween("count", Long.valueOf(1L), Long.valueOf(10L));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addBetweenDecimalSetsNotEmpty() {
		filter.addBetween("amount", new Decimal2("1.00"), new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDecimalNoSummarySetsNotEmpty() {
		filterNoSummary.addBetween("amount", new Decimal2("1.00"), new Decimal2("9.99"));
		assertFalse(filterNoSummary.isEmpty());
	}

	// --- addIn / addNotIn ---

	@Test
	void addInSetsNotEmpty() {
		filter.addIn("name", "a", "b", "c");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addInNoSummarySetsNotEmpty() {
		filterNoSummary.addIn("name", "x");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotInDoesNotSetNotEmpty() {
		filter.addNotIn("name", "a", "b");
		assertTrue(filter.isEmpty());
	}

	@Test
	void addNotInNoSummaryDoesNotSetNotEmpty() {
		filterNoSummary.addNotIn("name", "x");
		assertTrue(filterNoSummary.isEmpty());
	}

	// --- geometry filters ---

	@Test
	void addWithinSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addWithin("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addWithinNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addWithin("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addContainsGeometrySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addContains("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addContainsGeometryNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addContains("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addCrossesSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addCrosses("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addCrossesNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addCrosses("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addDisjointSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addDisjoint("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addDisjointNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addDisjoint("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addIntersectsSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addIntersects("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addIntersectsNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addIntersects("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addOverlapsSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addOverlaps("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOverlapsNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addOverlaps("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addTouchesSetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filter.addTouches("location", point);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTouchesNoSummarySetsNotEmpty() {
		Point point = new GeometryFactory().createPoint(new Coordinate(1.0, 2.0));
		filterNoSummary.addTouches("location", point);
		assertFalse(filterNoSummary.isEmpty());
	}
}
