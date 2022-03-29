package org.skyve.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;

public class TimeTest {

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testAddDays() {
		// setup the test data
		DateOnly initial = new DateOnly();
		DateOnly newTime = new DateOnly(initial);

		// validate the test data
		assertThat(initial, is(newTime));

		// call the method under test
		Time.addDays(newTime, 1);

		// verify the result
		long diff = (newTime.getTime() - initial.getTime());
		long days = diff / (24 * 60 * 60 * 1000);
		assertThat(days, is(1L));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testAddHours() {
		// setup the test data
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);

		// validate the test data
		assertThat(initial, is(newTime));

		// call the method under test
		Time.addHours(newTime, 1);

		// verify the result
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		long hours = secs / 3600;
		assertThat(hours, is(1L));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testAddMinutes() {
		// setup the test data
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);

		// validate the test data
		assertThat(initial, is(newTime));

		// call the method under test
		Time.addMinutes(newTime, 1);

		// verify the result
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		long mins = secs / 60;
		assertThat(mins, is(1L));
	}

	@Test
	@SuppressWarnings({ "boxing", "static-method" })
	public void testAddSeconds() {
		// setup the test data
		DateTime initial = new DateTime();
		DateTime newTime = new DateTime(initial);

		// validate the test data
		assertThat(initial, is(newTime));

		// call the method under test
		Time.addSeconds(newTime, 1);

		// verify the result
		long secs = (newTime.getTime() - initial.getTime()) / 1000;
		assertThat(secs, is(1L));
	}
}
