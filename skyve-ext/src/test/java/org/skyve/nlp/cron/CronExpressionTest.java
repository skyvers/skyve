package org.skyve.nlp.cron;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class CronExpressionTest {

	@Test
	@SuppressWarnings("static-method")
	public void testHasNothingAllNullReturnsTrue() {
		// setup the test data
		CronExpression c = new CronExpression();

		// call the method under test
		boolean result = c.hasNothing();

		// verify the result
		assertThat(Boolean.valueOf(result), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testHasNothingOneValueReturnsFalse() {
		// setup the test data
		CronExpression c = new CronExpression(Integer.valueOf(1), null, null, null, null);

		// call the method under test
		boolean result = c.hasNothing();

		// verify the result
		assertThat(Boolean.valueOf(result), is(Boolean.FALSE));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testFluentSetters() {
		// setup the test data
		CronExpression c = new CronExpression();

		// validate the test data
		assertThat(c.getMinute(), is(nullValue()));
		assertThat(c.getHour(), is(nullValue()));
		assertThat(c.getDayNumber(), is(nullValue()));
		assertThat(c.getMonth(), is(nullValue()));
		assertThat(c.getDayOfWeek(), is(nullValue()));

		// call the method under test
		c.setMinute(1).setHour(2).setDayNumber(3).setMonth(4).setDayOfWeek(5);

		// verify the result
		assertThat(c.toString(), is("1 2 3 4 5"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToStringAllNullReturnsEveryMinute() {
		// setup the test data
		CronExpression c = new CronExpression();

		// call the method under test
		String result = c.toString();

		// verify the result
		assertThat(result, is("0 0 * * *"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testToStringValidValuesReturnsCorrectExpression() {
		// setup the test data
		CronExpression c = new CronExpression(2, 0, 3, null, 5);

		// call the method under test
		String result = c.toString();

		// verify the result
		assertThat(result, is("2 0 3 * 5"));
	}

}
