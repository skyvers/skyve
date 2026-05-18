package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NullTolerantComparatorTest {

	private NullTolerantComparator<String> comparator;

	@BeforeEach
	void setUp() {
		comparator = new NullTolerantComparator<>();
	}

	@Test
	void bothNullReturnsZero() {
		assertEquals(0, comparator.compare(null, null));
	}

	@Test
	void firstNullReturnsNegative() {
		assertTrue(comparator.compare(null, "a") < 0);
	}

	@Test
	void secondNullReturnsPositive() {
		assertTrue(comparator.compare("a", null) > 0);
	}

	@Test
	void equalValuesReturnsZero() {
		assertEquals(0, comparator.compare("abc", "abc"));
	}

	@Test
	void lessThanReturnsNegative() {
		assertTrue(comparator.compare("a", "b") < 0);
	}

	@Test
	void greaterThanReturnsPositive() {
		assertTrue(comparator.compare("b", "a") > 0);
	}

	@Test
	void integerNullNullReturnsZero() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertEquals(0, intComp.compare(null, null));
	}

	@Test
	void integerFirstNullReturnsNegative() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertTrue(intComp.compare(null, Integer.valueOf(5)) < 0);
	}

	@Test
	void integerSecondNullReturnsPositive() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertTrue(intComp.compare(Integer.valueOf(5), null) > 0);
	}

	@Test
	void integerLessReturnsNegative() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertTrue(intComp.compare(Integer.valueOf(1), Integer.valueOf(2)) < 0);
	}

	@Test
	void integerGreaterReturnsPositive() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertTrue(intComp.compare(Integer.valueOf(2), Integer.valueOf(1)) > 0);
	}

	@Test
	void integerEqualReturnsZero() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertEquals(0, intComp.compare(Integer.valueOf(42), Integer.valueOf(42)));
	}

	@Test
	void longFirstNullReturnsNegative() {
		NullTolerantComparator<Long> longComp = new NullTolerantComparator<>();
		assertTrue(longComp.compare(null, Long.valueOf(100L)) < 0);
	}

	@Test
	void longBothNullReturnsZero() {
		NullTolerantComparator<Long> longComp = new NullTolerantComparator<>();
		assertEquals(0, longComp.compare(null, null));
	}
}
