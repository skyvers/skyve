package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.skyve.domain.types.DateOnly;

public class TimeTest {
	@Test
	@SuppressWarnings("static-method")
	public void testCoalesce() {
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

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("static-method")
	public void testMaxNoArgsThrowsException() {
		// call the method under test
		Time.max();

		fail("Should throw error before this line");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMax() {
		// setup test data
		DateOnly today = new DateOnly();
		DateOnly tomorrow = Time.addDaysToNew(today, 1);
		DateOnly yesterday = Time.addDaysToNew(today, -1);

		// call the method under test
		DateOnly result = Time.max(today, tomorrow, yesterday);

		// verify the result
		assertThat(result, is(tomorrow));
	}

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("static-method")
	public void testMinNoArgsThrowsException() {
		// call the method under test
		Time.min();

		fail("Should throw error before this line");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testMin() {
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
