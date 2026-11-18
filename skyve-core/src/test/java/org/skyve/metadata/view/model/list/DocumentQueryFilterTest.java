package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.function.Consumer;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;

/**
 * Comprehensive tests for DocumentQueryFilter.
 * The constructor is package-private, so this test lives in the same package.
 */
@SuppressWarnings({ "java:S4144", "java:S8692" }) // system clock OK
class DocumentQueryFilterTest {
	private static final Consumer<String> NO_JOINS = binding -> { /* no list model to join */ };
	private static final String B = "binding";

	private DocumentFilter detailFilter;
	private DocumentFilter summaryFilter;
	private DocumentQueryFilter filter;
	private DocumentQueryFilter filterNoSummary;
	private List<String> filteredBindings;
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

		filteredBindings = new ArrayList<>();
		detailFilter = mock(DocumentFilter.class);
		summaryFilter = mock(DocumentFilter.class);
		filter = new DocumentQueryFilter(detailFilter, summaryFilter, filteredBindings::add);
		filterNoSummary = new DocumentQueryFilter(mock(DocumentFilter.class), null, NO_JOINS);

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
		DocumentQueryFilter other = new DocumentQueryFilter(otherDetail, otherSummary, NO_JOINS);
		filter.addAnd(other);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOrDelegatesToDetailAndSummaryFilters() {
		DocumentFilter otherDetail = mock(DocumentFilter.class);
		DocumentFilter otherSummary = mock(DocumentFilter.class);
		DocumentQueryFilter other = new DocumentQueryFilter(otherDetail, otherSummary, NO_JOINS);
		filter.addOr(other);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addAndWithNoSummaryDoesNotThrow() {
		DocumentQueryFilter other = new DocumentQueryFilter(mock(DocumentFilter.class), null, NO_JOINS);
		filterNoSummary.addAnd(other);
		assertFalse(filterNoSummary.isEmpty());
	}

	@Test
	void addOrWithNoSummaryDoesNotThrow() {
		DocumentQueryFilter other = new DocumentQueryFilter(mock(DocumentFilter.class), null, NO_JOINS);
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

	/**
	 * Each condition on a binding is the same call on the detail and the summary filter, and tells the binding it is
	 * on, whether or not it also matches no value: under Match Any a positive condition through an association must not
	 * drop the rows that the other criteria keep.
	 */
	@Test
	void everyConditionOnABindingReachesBothFiltersAndTellsTheBinding() {
		Date date = new Date();
		Decimal2 decimal = new Decimal2("1.5");
		Point point = new GeometryFactory().createPoint(new Coordinate(0, 0));
		Integer one = Integer.valueOf(1);
		Integer two = Integer.valueOf(2);
		Long longOne = Long.valueOf(1L);
		Long longTwo = Long.valueOf(2L);
		List<Executable> checks = List.of(
			check("addNull", f -> f.addNull(B), d -> d.addNull(B)),
			check("addNotNull", f -> f.addNotNull(B), d -> d.addNotNull(B)),
			check("addEquals String", f -> f.addEquals(B, "v"), d -> d.addEquals(B, "v")),
			check("addEquals Date", f -> f.addEquals(B, date), d -> d.addEquals(B, date)),
			check("addEquals Integer", f -> f.addEquals(B, one), d -> d.addEquals(B, one)),
			check("addEquals Long", f -> f.addEquals(B, longOne), d -> d.addEquals(B, longOne)),
			check("addEquals Decimal", f -> f.addEquals(B, decimal), d -> d.addEquals(B, decimal)),
			check("addEquals Boolean", f -> f.addEquals(B, Boolean.TRUE), d -> d.addEquals(B, Boolean.TRUE)),
			check("addEquals Enum", f -> f.addEquals(B, SortDirection.ascending), d -> d.addEquals(B, SortDirection.ascending)),
			check("addEquals Geometry", f -> f.addEquals(B, point), d -> d.addEquals(B, point)),
			check("addNotEquals String", f -> f.addNotEquals(B, "v"), d -> d.addNotEquals(B, "v")),
			check("addNotEquals Date", f -> f.addNotEquals(B, date), d -> d.addNotEquals(B, date)),
			check("addNotEquals Integer", f -> f.addNotEquals(B, one), d -> d.addNotEquals(B, one)),
			check("addNotEquals Long", f -> f.addNotEquals(B, longOne), d -> d.addNotEquals(B, longOne)),
			check("addNotEquals Decimal", f -> f.addNotEquals(B, decimal), d -> d.addNotEquals(B, decimal)),
			check("addNotEquals Boolean", f -> f.addNotEquals(B, Boolean.TRUE), d -> d.addNotEquals(B, Boolean.TRUE)),
			check("addNotEquals Enum", f -> f.addNotEquals(B, SortDirection.ascending), d -> d.addNotEquals(B, SortDirection.ascending)),
			check("addNotEquals Geometry", f -> f.addNotEquals(B, point), d -> d.addNotEquals(B, point)),
			check("addEqualsIgnoreCase", f -> f.addEqualsIgnoreCase(B, "v"), d -> d.addLike(B, "v")),
			check("addNotEqualsIgnoreCase", f -> f.addNotEqualsIgnoreCase(B, "v"), d -> d.addNotLike(B, "v")),
			check("addContains", f -> f.addContains(B, "v"), d -> d.addLike(B, "%v%")),
			check("addNotContains", f -> f.addNotContains(B, "v"), d -> d.addNullOrNotLike(B, "%v%")),
			check("addStartsWith", f -> f.addStartsWith(B, "v"), d -> d.addLike(B, "v%")),
			check("addNotStartsWith", f -> f.addNotStartsWith(B, "v"), d -> d.addNullOrNotLike(B, "v%")),
			check("addEndsWith", f -> f.addEndsWith(B, "v"), d -> d.addLike(B, "%v")),
			check("addNotEndsWith", f -> f.addNotEndsWith(B, "v"), d -> d.addNullOrNotLike(B, "%v")),
			check("addGreaterThan String", f -> f.addGreaterThan(B, "v"), d -> d.addGreaterThan(B, "v")),
			check("addGreaterThan Date", f -> f.addGreaterThan(B, date), d -> d.addGreaterThan(B, date)),
			check("addGreaterThan Integer", f -> f.addGreaterThan(B, one), d -> d.addGreaterThan(B, one)),
			check("addGreaterThan Long", f -> f.addGreaterThan(B, longOne), d -> d.addGreaterThan(B, longOne)),
			check("addGreaterThan Decimal", f -> f.addGreaterThan(B, decimal), d -> d.addGreaterThan(B, decimal)),
			check("addGreaterThanOrEqualTo String", f -> f.addGreaterThanOrEqualTo(B, "v"), d -> d.addGreaterThanOrEqualTo(B, "v")),
			check("addGreaterThanOrEqualTo Date", f -> f.addGreaterThanOrEqualTo(B, date), d -> d.addGreaterThanOrEqualTo(B, date)),
			check("addGreaterThanOrEqualTo Integer", f -> f.addGreaterThanOrEqualTo(B, one), d -> d.addGreaterThanOrEqualTo(B, one)),
			check("addGreaterThanOrEqualTo Long", f -> f.addGreaterThanOrEqualTo(B, longOne), d -> d.addGreaterThanOrEqualTo(B, longOne)),
			check("addGreaterThanOrEqualTo Decimal", f -> f.addGreaterThanOrEqualTo(B, decimal), d -> d.addGreaterThanOrEqualTo(B, decimal)),
			check("addLessThan String", f -> f.addLessThan(B, "v"), d -> d.addLessThan(B, "v")),
			check("addLessThan Date", f -> f.addLessThan(B, date), d -> d.addLessThan(B, date)),
			check("addLessThan Integer", f -> f.addLessThan(B, one), d -> d.addLessThan(B, one)),
			check("addLessThan Long", f -> f.addLessThan(B, longOne), d -> d.addLessThan(B, longOne)),
			check("addLessThan Decimal", f -> f.addLessThan(B, decimal), d -> d.addLessThan(B, decimal)),
			check("addLessThanOrEqualTo String", f -> f.addLessThanOrEqualTo(B, "v"), d -> d.addLessThanOrEqualTo(B, "v")),
			check("addLessThanOrEqualTo Date", f -> f.addLessThanOrEqualTo(B, date), d -> d.addLessThanOrEqualTo(B, date)),
			check("addLessThanOrEqualTo Integer", f -> f.addLessThanOrEqualTo(B, one), d -> d.addLessThanOrEqualTo(B, one)),
			check("addLessThanOrEqualTo Long", f -> f.addLessThanOrEqualTo(B, longOne), d -> d.addLessThanOrEqualTo(B, longOne)),
			check("addLessThanOrEqualTo Decimal", f -> f.addLessThanOrEqualTo(B, decimal), d -> d.addLessThanOrEqualTo(B, decimal)),
			check("addBetween String", f -> f.addBetween(B, "a", "b"), d -> d.addBetween(B, "a", "b")),
			check("addBetween Date", f -> f.addBetween(B, date, date), d -> d.addBetween(B, date, date)),
			check("addBetween Integer", f -> f.addBetween(B, one, two), d -> d.addBetween(B, one, two)),
			check("addBetween Long", f -> f.addBetween(B, longOne, longTwo), d -> d.addBetween(B, longOne, longTwo)),
			check("addBetween Decimal", f -> f.addBetween(B, decimal, decimal), d -> d.addBetween(B, decimal, decimal)),
			check("addIn", f -> f.addIn(B, one, two), d -> d.addIn(B, one, two)),
			check("addNotIn", f -> f.addNotIn(B, one, two), d -> d.addNotIn(B, one, two)),
			check("addWithin", f -> f.addWithin(B, point), d -> d.addWithin(B, point)),
			check("addContains Geometry", f -> f.addContains(B, point), d -> d.addContains(B, point)),
			check("addCrosses", f -> f.addCrosses(B, point), d -> d.addCrosses(B, point)),
			check("addDisjoint", f -> f.addDisjoint(B, point), d -> d.addDisjoint(B, point)),
			check("addIntersects", f -> f.addIntersects(B, point), d -> d.addIntersects(B, point)),
			check("addOverlaps", f -> f.addOverlaps(B, point), d -> d.addOverlaps(B, point)),
			check("addTouches", f -> f.addTouches(B, point), d -> d.addTouches(B, point)));
		assertAll(checks);
	}

	/** In an empty set selects nothing (null and not null), not in an empty set every row; both tell the binding. */
	@Test
	void anEmptyInOrNotInTellsTheBinding() {
		assertAll(check("addIn none", f -> f.addIn(B), d -> d.addNull(B), d -> d.addNotNull(B)),
				check("addNotIn none", f -> f.addNotIn(B), d -> d.addExpression("1 = 1")));
	}

	/** Tags are on the row itself, and an and or an or of filters was told by its own conditions. */
	@Test
	void theTaggedAndCompoundConditionsTellNoBinding() {
		filter.addTagged("tag-1", true);
		filter.addAnd(new DocumentQueryFilter(mock(DocumentFilter.class), mock(DocumentFilter.class), NO_JOINS));
		filter.addOr(new DocumentQueryFilter(mock(DocumentFilter.class), mock(DocumentFilter.class), NO_JOINS));
		assertEquals(List.of(), filteredBindings);
	}

	/**
	 * Checks that a condition makes the expected calls (one each) on a new detail and summary filter and nothing else,
	 * tells only its binding, and leaves the filter not empty.
	 */
	@SafeVarargs
	private static Executable check(String name, Consumer<DocumentQueryFilter> condition, Consumer<DocumentFilter>... expected) {
		return () -> {
			DocumentFilter detail = mock(DocumentFilter.class);
			DocumentFilter summary = mock(DocumentFilter.class);
			List<String> told = new ArrayList<>(1);
			DocumentQueryFilter tested = new DocumentQueryFilter(detail, summary, told::add);
			condition.accept(tested);
			for (Consumer<DocumentFilter> call : expected) {
				call.accept(verify(detail));
				call.accept(verify(summary));
			}
			verifyNoMoreInteractions(detail, summary);
			assertEquals(List.of(B), told, name);
			assertFalse(tested.isEmpty(), name);
		};
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
