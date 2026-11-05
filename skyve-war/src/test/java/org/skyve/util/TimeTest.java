package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;

class TimeTest {
	@Test
	@SuppressWarnings("static-method")
	void testCoalesce() {
		// setup test data
		DateOnly today = new DateOnly();
		DateOnly tomorrow = Time.addDaysToNew(today, 1);

		// call the method under test
		DateOnly result = Time.coalesce(today, tomorrow);
		DateOnly result2 = Time.coalesce(null, tomorrow);

		// verify the result
		assertThat(result, is(today));
		assertThat(result2, is(tomorrow));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMaxNoArgsThrowsException() {
		// call the method under test
		assertThrows(IllegalArgumentException.class, () -> Time.max());
	}

	@Test
	@SuppressWarnings("static-method")
	void testMax() {
		// setup test data
		DateOnly today = new DateOnly();
		DateOnly tomorrow = Time.addDaysToNew(today, 1);
		DateOnly yesterday = Time.addDaysToNew(today, -1);

		// call the method under test
		DateOnly result = Time.max(today, tomorrow, yesterday);

		// verify the result
		assertThat(result, is(tomorrow));
	}

	@Test
	@SuppressWarnings("static-method")
	void testMinNoArgsThrowsException() {
		// call the method under test
		assertThrows(IllegalArgumentException.class, () -> Time.min());
	}

	@Test
	@SuppressWarnings("static-method")
	void testMin() {
		// setup test data
		DateOnly today = new DateOnly();
		DateOnly tomorrow = Time.addDaysToNew(today, 1);
		DateOnly yesterday = Time.addDaysToNew(today, -1);

		// call the method under test
		DateOnly result = Time.min(today, tomorrow, yesterday);

		// verify the result
		assertThat(result, is(yesterday));
	}
}
