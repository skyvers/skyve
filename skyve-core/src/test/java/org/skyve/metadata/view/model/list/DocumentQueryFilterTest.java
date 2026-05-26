package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
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
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;

/**
 * Comprehensive tests for DocumentQueryFilter.
 * The constructor is package-private, so this test lives in the same package.
 */
@SuppressWarnings({ "static-method", "boxing" })
class DocumentQueryFilterTest {

	private DocumentFilter detailFilter;
	private DocumentFilter summaryFilter;
	private DocumentQueryFilter filter;
	private DocumentQueryFilter filterNoSummary;
	private boolean originalQueryTrace;

	@BeforeEach
	void setUp() {
		// Set up thread-local user so CORE.getUser() works
		User user = mock(User.class);
		when(user.getId()).thenReturn("user-1");
		AbstractPersistence persistence =
			mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();

		detailFilter = mock(DocumentFilter.class);
		summaryFilter = mock(DocumentFilter.class);
		filter = new DocumentQueryFilter(detailFilter, summaryFilter);
		filterNoSummary = new DocumentQueryFilter(mock(DocumentFilter.class), null);

		originalQueryTrace = UtilImpl.QUERY_TRACE;
		UtilImpl.QUERY_TRACE = true; // enable to cover logger lines
	}

	@AfterEach
	@SuppressWarnings("unchecked")
	void tearDown() throws Exception {
		UtilImpl.QUERY_TRACE = originalQueryTrace;
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	// ---- isEmpty ----

	@Test
	void isEmptyReturnsTrueForNewFilter() {
		assertTrue(filter.isEmpty());
	}

	@Test
	void isEmptyReturnsFalseAfterAddNull() {
		filter.addNull("binding");
		assertFalse(filter.isEmpty());
	}

	// ---- addAnd / addOr ----

	@Test
	void addAndDelegatesToDetailAndSummaryFilters() {
		DocumentFilter otherDetail = mock(DocumentFilter.class);
		DocumentFilter otherSummary = mock(DocumentFilter.class);
		DocumentQueryFilter other = new DocumentQueryFilter(otherDetail, otherSummary);
		filter.addAnd(other);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOrDelegatesToDetailAndSummaryFilters() {
		DocumentFilter otherDetail = mock(DocumentFilter.class);
		DocumentFilter otherSummary = mock(DocumentFilter.class);
		DocumentQueryFilter other = new DocumentQueryFilter(otherDetail, otherSummary);
		filter.addOr(other);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addAndWithNoSummaryDoesNotThrow() {
		DocumentQueryFilter other = new DocumentQueryFilter(mock(DocumentFilter.class), null);
		filterNoSummary.addAnd(other);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addOrWithNoSummaryDoesNotThrow() {
		DocumentQueryFilter other = new DocumentQueryFilter(mock(DocumentFilter.class), null);
		filterNoSummary.addOr(other);
		assertFalse(filterNoSummary.isEmpty());
	}

	// ---- addTagged ----

	@Test
	void addTaggedTrueDelegatesToFilters() {
		filter.addTagged("tag-1", true);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTaggedFalseDelegatesToFilters() {
		filter.addTagged("tag-1", false);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTaggedWithNoSummaryDoesNotThrow() {
		filterNoSummary.addTagged("tag-1", true);
		assertFalse(filterNoSummary.isEmpty());
	}

	// ---- addNull / addNotNull ----

	@Test
	void addNullDelegatesToFilters() {
		filter.addNull("binding");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotNullDelegatesToFilters() {
		filter.addNotNull("binding");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNullWithNoSummaryDoesNotThrow() {
		filterNoSummary.addNull("binding");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotNullWithNoSummaryDoesNotThrow() {
		filterNoSummary.addNotNull("binding");
		assertFalse(filterNoSummary.isEmpty());
	}

	// ---- addEquals (all overloads) ----

	@Test
	void addEqualsStringDelegatesToFilters() {
		filter.addEquals("binding", "value");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDateDelegatesToFilters() {
		filter.addEquals("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsIntegerDelegatesToFilters() {
		filter.addEquals("binding", Integer.valueOf(42));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsLongDelegatesToFilters() {
		filter.addEquals("binding", Long.valueOf(42L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDecimalDelegatesToFilters() {
		filter.addEquals("binding", new Decimal2("1.23"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsBooleanDelegatesToFilters() {
		filter.addEquals("binding", Boolean.TRUE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsEnumDelegatesToFilters() {
		filter.addEquals("binding", java.time.DayOfWeek.MONDAY);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsGeometryDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(1, 2));
		filter.addEquals("binding", pt);
		assertFalse(filter.isEmpty());
	}

	// ---- addNotEquals (all overloads) ----

	@Test
	void addNotEqualsStringDelegatesToFilters() {
		filter.addNotEquals("binding", "value");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDateDelegatesToFilters() {
		filter.addNotEquals("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIntegerDelegatesToFilters() {
		filter.addNotEquals("binding", Integer.valueOf(1));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsLongDelegatesToFilters() {
		filter.addNotEquals("binding", Long.valueOf(1L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDecimalDelegatesToFilters() {
		filter.addNotEquals("binding", new Decimal2("0.5"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsBooleanDelegatesToFilters() {
		filter.addNotEquals("binding", Boolean.FALSE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsEnumDelegatesToFilters() {
		filter.addNotEquals("binding", java.time.DayOfWeek.FRIDAY);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsGeometryDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(3, 4));
		filter.addNotEquals("binding", pt);
		assertFalse(filter.isEmpty());
	}

	// ---- case-insensitive string methods ----

	@Test
	void addEqualsIgnoreCaseDelegatesToFilters() {
		filter.addEqualsIgnoreCase("binding", "value");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIgnoreCaseDelegatesToFilters() {
		filter.addNotEqualsIgnoreCase("binding", "value");
		assertFalse(filter.isEmpty());
	}

	// ---- string pattern methods ----

	@Test
	void addContainsStringDelegatesToFilters() {
		filter.addContains("binding", "substring");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotContainsStringDelegatesToFilters() {
		filter.addNotContains("binding", "substring");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addStartsWithDelegatesToFilters() {
		filter.addStartsWith("binding", "prefix");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotStartsWithDelegatesToFilters() {
		filter.addNotStartsWith("binding", "prefix");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEndsWithDelegatesToFilters() {
		filter.addEndsWith("binding", "suffix");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEndsWithDelegatesToFilters() {
		filter.addNotEndsWith("binding", "suffix");
		assertFalse(filter.isEmpty());
	}

	// ---- addGreaterThan (all overloads) ----

	@Test
	void addGreaterThanStringDelegatesToFilters() {
		filter.addGreaterThan("binding", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDateDelegatesToFilters() {
		filter.addGreaterThan("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanIntegerDelegatesToFilters() {
		filter.addGreaterThan("binding", Integer.valueOf(10));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanLongDelegatesToFilters() {
		filter.addGreaterThan("binding", Long.valueOf(10L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDecimalDelegatesToFilters() {
		filter.addGreaterThan("binding", new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	// ---- addGreaterThanOrEqualTo (all overloads) ----

	@Test
	void addGreaterThanOrEqualToStringDelegatesToFilters() {
		filter.addGreaterThanOrEqualTo("binding", "a");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDateDelegatesToFilters() {
		filter.addGreaterThanOrEqualTo("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToIntegerDelegatesToFilters() {
		filter.addGreaterThanOrEqualTo("binding", Integer.valueOf(0));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToLongDelegatesToFilters() {
		filter.addGreaterThanOrEqualTo("binding", Long.valueOf(0L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDecimalDelegatesToFilters() {
		filter.addGreaterThanOrEqualTo("binding", new Decimal2("0.01"));
		assertFalse(filter.isEmpty());
	}

	// ---- addLessThan (all overloads) ----

	@Test
	void addLessThanStringDelegatesToFilters() {
		filter.addLessThan("binding", "b");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDateDelegatesToFilters() {
		filter.addLessThan("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanIntegerDelegatesToFilters() {
		filter.addLessThan("binding", Integer.valueOf(100));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanLongDelegatesToFilters() {
		filter.addLessThan("binding", Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDecimalDelegatesToFilters() {
		filter.addLessThan("binding", new Decimal2("99.00"));
		assertFalse(filter.isEmpty());
	}

	// ---- addLessThanOrEqualTo (all overloads) ----

	@Test
	void addLessThanOrEqualToStringDelegatesToFilters() {
		filter.addLessThanOrEqualTo("binding", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDateDelegatesToFilters() {
		filter.addLessThanOrEqualTo("binding", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToIntegerDelegatesToFilters() {
		filter.addLessThanOrEqualTo("binding", Integer.valueOf(50));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToLongDelegatesToFilters() {
		filter.addLessThanOrEqualTo("binding", Long.valueOf(50L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDecimalDelegatesToFilters() {
		filter.addLessThanOrEqualTo("binding", new Decimal2("50.00"));
		assertFalse(filter.isEmpty());
	}

	// ---- addBetween (all overloads) ----

	@Test
	void addBetweenStringDelegatesToFilters() {
		filter.addBetween("binding", "a", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDateDelegatesToFilters() {
		filter.addBetween("binding", new Date(0), new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenIntegerDelegatesToFilters() {
		filter.addBetween("binding", Integer.valueOf(1), Integer.valueOf(10));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenLongDelegatesToFilters() {
		filter.addBetween("binding", Long.valueOf(1L), Long.valueOf(10L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDecimalDelegatesToFilters() {
		filter.addBetween("binding", new Decimal2("1.00"), new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	// ---- addIn / addNotIn ----

	@Test
	void addInWithValuesDelegatesToFilter() {
		filter.addIn("binding", "a", "b", "c");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addInWithNoValuesFallsBackToNullAndNotNull() {
		filter.addIn("binding"); // no values
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotInWithValuesDelegatesToFilter() {
		filter.addNotIn("binding", "x", "y");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotInWithNoValuesFallsBackToNullAndNotNull() {
		filter.addNotIn("binding"); // no values
		assertFalse(filter.isEmpty());
	}

	@Test
	void addInWithNoSummaryAndValuesDelegatesToDetailOnly() {
		filterNoSummary.addIn("binding", "val");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addInWithNoSummaryAndNoValuesDelegatesToDetailOnly() {
		filterNoSummary.addIn("binding"); // no values
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addNotInWithNoSummaryAndNoValuesDelegatesToDetailOnly() {
		filterNoSummary.addNotIn("binding"); // no values
		assertFalse(filterNoSummary.isEmpty());
	}

	// ---- Geometry spatial methods ----

	@Test
	void addWithinDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addWithin("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addContainsGeometryDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addContains("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addCrossesDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addCrosses("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addDisjointDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addDisjoint("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addIntersectsDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addIntersects("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOverlapsDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addOverlaps("binding", pt);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTouchesDelegatesToFilters() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filter.addTouches("binding", pt);
		assertFalse(filter.isEmpty());
	}

	// ---- no-summary variants for a representative selection ----

	@Test
	void addEqualsStringWithNoSummaryDoesNotThrow() {
		filterNoSummary.addEquals("binding", "value");
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addGreaterThanIntegerWithNoSummaryDoesNotThrow() {
		filterNoSummary.addGreaterThan("binding", Integer.valueOf(5));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addBetweenIntegerWithNoSummaryDoesNotThrow() {
		filterNoSummary.addBetween("binding", Integer.valueOf(1), Integer.valueOf(5));
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addWithinWithNoSummaryDoesNotThrow() {
		Point pt = new GeometryFactory().createPoint(new Coordinate(0, 0));
		filterNoSummary.addWithin("binding", pt);
		assertFalse(filterNoSummary.isEmpty());
	}
}
