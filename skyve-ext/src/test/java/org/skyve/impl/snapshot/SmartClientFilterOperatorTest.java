package org.skyve.impl.snapshot;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.metadata.FilterOperator;

@SuppressWarnings("static-method")
public class SmartClientFilterOperatorTest {

	// ---- fromFilterOperator mappings ----

	@Test
	public void equalMapsToEquals() {
		assertEquals(SmartClientFilterOperator.equals,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.equal));
	}

	@Test
	public void greaterMapsToGreaterThan() {
		assertEquals(SmartClientFilterOperator.greaterThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.greater));
	}

	@Test
	public void greaterEqualMapsToGreaterOrEqual() {
		assertEquals(SmartClientFilterOperator.greaterOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.greaterEqual));
	}

	@Test
	public void isNullMapsToIsNull() {
		assertEquals(SmartClientFilterOperator.isNull,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.isNull));
	}

	@Test
	public void lessMapsToLessThan() {
		assertEquals(SmartClientFilterOperator.lessThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.less));
	}

	@Test
	public void lessEqualMapsToLessOrEqual() {
		assertEquals(SmartClientFilterOperator.lessOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.lessEqual));
	}

	@Test
	public void likeMapsToIContains() {
		assertEquals(SmartClientFilterOperator.iContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.like));
	}

	@Test
	public void notEqualMapsToINotEqual() {
		assertEquals(SmartClientFilterOperator.iNotEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notEqual));
	}

	@Test
	public void notLikeMapsToINotContains() {
		assertEquals(SmartClientFilterOperator.iNotContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notLike));
	}

	@Test
	public void notNullMapsToNotNull() {
		assertEquals(SmartClientFilterOperator.notNull,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.notNull));
	}

	@Test
	public void nullOrEqualMapsToEquals() {
		assertEquals(SmartClientFilterOperator.equals,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrEqual));
	}

	@Test
	public void nullOrGreaterMapsToGreaterThan() {
		assertEquals(SmartClientFilterOperator.greaterThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrGreater));
	}

	@Test
	public void nullOrGreaterEqualMapsToGreaterOrEqual() {
		assertEquals(SmartClientFilterOperator.greaterOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrGreaterEqual));
	}

	@Test
	public void nullOrLessMapsToLessThan() {
		assertEquals(SmartClientFilterOperator.lessThan,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLess));
	}

	@Test
	public void nullOrLessEqualMapsToLessOrEqual() {
		assertEquals(SmartClientFilterOperator.lessOrEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLessEqual));
	}

	@Test
	public void nullOrLikeMapsToIContains() {
		assertEquals(SmartClientFilterOperator.iContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrLike));
	}

	@Test
	public void nullOrNotEqualMapsToNotEqual() {
		assertEquals(SmartClientFilterOperator.notEqual,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrNotEqual));
	}

	@Test
	public void nullOrNotLikeMapsToINotContains() {
		assertEquals(SmartClientFilterOperator.iNotContains,
				SmartClientFilterOperator.fromFilterOperator(FilterOperator.nullOrNotLike));
	}

	// ---- enum constants sanity ----

	@Test
	public void valuesArrayIsNonEmpty() {
		assertTrue(SmartClientFilterOperator.values().length > 0);
	}

	@Test
	public void valueOfSubstringIsSubstring() {
		assertEquals(SmartClientFilterOperator.substring,
				SmartClientFilterOperator.valueOf("substring"));
	}

	private static void assertTrue(boolean condition) {
		org.junit.Assert.assertTrue(condition);
	}
}
