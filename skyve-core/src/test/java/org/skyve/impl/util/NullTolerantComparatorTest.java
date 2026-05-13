package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(comparator.compare(null, null), is(0));
	}

	@Test
	void firstNullReturnsNegative() {
		assertThat(comparator.compare(null, "a") < 0, is(true));
	}

	@Test
	void secondNullReturnsPositive() {
		assertThat(comparator.compare("a", null) > 0, is(true));
	}

	@Test
	void equalValuesReturnsZero() {
		assertThat(comparator.compare("abc", "abc"), is(0));
	}

	@Test
	void lessThanReturnsNegative() {
		assertThat(comparator.compare("a", "b") < 0, is(true));
	}

	@Test
	void greaterThanReturnsPositive() {
		assertThat(comparator.compare("b", "a") > 0, is(true));
	}

	@Test
	void integerNullNullReturnsZero() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(null, null), is(0));
	}

	@Test
	void integerFirstNullReturnsNegative() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(null, Integer.valueOf(5)) < 0, is(true));
	}

	@Test
	void integerSecondNullReturnsPositive() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(Integer.valueOf(5), null) > 0, is(true));
	}

	@Test
	void integerLessReturnsNegative() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(Integer.valueOf(1), Integer.valueOf(2)) < 0, is(true));
	}

	@Test
	void integerGreaterReturnsPositive() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(Integer.valueOf(2), Integer.valueOf(1)) > 0, is(true));
	}

	@Test
	void integerEqualReturnsZero() {
		NullTolerantComparator<Integer> intComp = new NullTolerantComparator<>();
		assertThat(intComp.compare(Integer.valueOf(42), Integer.valueOf(42)), is(0));
	}

	@Test
	void longFirstNullReturnsNegative() {
		NullTolerantComparator<Long> longComp = new NullTolerantComparator<>();
		assertThat(longComp.compare(null, Long.valueOf(100L)) < 0, is(true));
	}

	@Test
	void longBothNullReturnsZero() {
		NullTolerantComparator<Long> longComp = new NullTolerantComparator<>();
		assertThat(longComp.compare(null, null), is(0));
	}
}
