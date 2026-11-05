package org.skyve.impl.snapshot;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.FilterOperator;

@SuppressWarnings("static-method")
class SmartClientFilterOperatorTest {

	// ---- fromFilterOperator mappings ----

	@Test
	void equalMapsToEquals() {
		assertEquals(SmartClientFilterOperator.equals,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.equal));
	}

	@Test
	void greaterMapsToGreaterThan() {
		assertEquals(SmartClientFilterOperator.greaterThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.greater));
	}

	@Test
	void greaterEqualMapsToGreaterOrEqual() {
		assertEquals(SmartClientFilterOperator.greaterOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.greaterEqual));
	}

	@Test
	void isNullMapsToIsNull() {
		assertEquals(SmartClientFilterOperator.isNull,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.isNull));
	}

	@Test
	void lessMapsToLessThan() {
		assertEquals(SmartClientFilterOperator.lessThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.less));
	}

	@Test
	void lessEqualMapsToLessOrEqual() {
		assertEquals(SmartClientFilterOperator.lessOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.lessEqual));
	}

	@Test
	void likeMapsToIContains() {
		assertEquals(SmartClientFilterOperator.iContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.like));
	}

	@Test
	void notEqualMapsToINotEqual() {
		assertEquals(SmartClientFilterOperator.iNotEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notEqual));
	}

	@Test
	void notLikeMapsToINotContains() {
		assertEquals(SmartClientFilterOperator.iNotContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notLike));
	}

	@Test
	void notNullMapsToNotNull() {
		assertEquals(SmartClientFilterOperator.notNull,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notNull));
	}

	@Test
	void nullOrEqualMapsToEquals() {
		assertEquals(SmartClientFilterOperator.equals,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrEqual));
	}

	@Test
	void nullOrGreaterMapsToGreaterThan() {
		assertEquals(SmartClientFilterOperator.greaterThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrGreater));
	}

	@Test
	void nullOrGreaterEqualMapsToGreaterOrEqual() {
		assertEquals(SmartClientFilterOperator.greaterOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrGreaterEqual));
	}

	@Test
	void nullOrLessMapsToLessThan() {
		assertEquals(SmartClientFilterOperator.lessThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLess));
	}

	@Test
	void nullOrLessEqualMapsToLessOrEqual() {
		assertEquals(SmartClientFilterOperator.lessOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLessEqual));
	}

	@Test
	void nullOrLikeMapsToIContains() {
		assertEquals(SmartClientFilterOperator.iContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLike));
	}

	@Test
	void nullOrNotEqualMapsToNotEqual() {
		assertEquals(SmartClientFilterOperator.notEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrNotEqual));
	}

	@Test
	void nullOrNotLikeMapsToINotContains() {
		assertEquals(SmartClientFilterOperator.iNotContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrNotLike));
	}

	// ---- enum constants sanity ----

	@Test
	void valuesArrayIsNonEmpty() {
		assertTrue(SmartClientFilterOperator.values().length > 0);
	}

	@Test
	void valueOfSubstringIsSubstring() {
		assertEquals(SmartClientFilterOperator.substring,
				SmartClientFilterOperator.valueOf("substring"));
	}

	private static void assertTrue(boolean condition) {
		org.junit.jupiter.api.Assertions.assertTrue(condition);
	}
}
