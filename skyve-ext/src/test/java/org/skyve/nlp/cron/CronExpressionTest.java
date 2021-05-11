package org.skyve.nlp.cron;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.fail;

import org.junit.Test;

public class CronExpressionTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testFluentSetters() {
		// setup the test data
		CronExpression c = new CronExpression();

		// validate the test data
		assertThat(c.getSecond(), is(nullValue()));
		assertThat(c.getMinute(), is(nullValue()));
		assertThat(c.getHour(), is(nullValue()));
		assertThat(c.getDayNumber(), is(nullValue()));
		assertThat(c.getMonth(), is(nullValue()));
		assertThat(c.getDayOfWeek(), is(nullValue()));

		// call the method under test
		c.setSecond(0).setMinute(1).setHour(2).setDayNumber(3).setMonth(4).setDayOfWeek(5);

		// verify the result
		assertThat(c.toString(), is("0 1 2 3 4 5"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testFromExpressionNullString() {
		// call the method under test
		CronExpression c = CronExpression.fromExpression(null);

		// verify the result
		assertThat(c, is(nullValue()));
	}

	@Test(expected = CronParserException.class)
	@SuppressWarnings("static-method")
	public void testFromExpressionTooFewParametersThrowsException() {
		// setup the test data
		String expression = "* * * *";

		// call the method under test
		CronExpression.fromExpression(expression);

		fail("Should throw exception before this line");
	}

	@Test(expected = CronParserException.class)
	@SuppressWarnings("static-method")
	public void testFromExpressionTooManyParametersThrowsException() {
		// setup the test data
		String expression = "* * * * * * *";

		// call the method under test
		CronExpression.fromExpression(expression);

		fail("Should throw exception before this line");
	}

	@Test
	@SuppressWarnings("static-method")
	public void testFromExpressionValidParameters() {
		// setup the test data
		String expression = "* * * * * *";

		// call the method under test
		CronExpression c = CronExpression.fromExpression(expression);

		// verify the result
		assertThat(c.getSecond(), is("*"));
		assertThat(c.getMinute(), is("*"));
		assertThat(c.getHour(), is("*"));
		assertThat(c.getDayNumber(), is("*"));
		assertThat(c.getMonth(), is("*"));
		assertThat(c.getDayOfWeek(), is("*"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testFromExpressionValidParametersWithSteps() {
		// setup the test data
		String expression = "0 0 */6 * * *";

		// call the method under test
		CronExpression c = CronExpression.fromExpression(expression);

		// verify the result
		assertThat(c.getSecond(), is("0"));
		assertThat(c.getMinute(), is("0"));
		assertThat(c.getHour(), is("*/6"));
		assertThat(c.getDayNumber(), is("*"));
		assertThat(c.getMonth(), is("*"));
		assertThat(c.getDayOfWeek(), is("*"));
	}

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
		CronExpression c = new CronExpression(null, Integer.valueOf(1), null, null, null, null);

		// call the method under test
		boolean result = c.hasNothing();

		// verify the result
		assertThat(Boolean.valueOf(result), is(Boolean.FALSE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageNoExpressionReturnsNull() throws Exception {
		// setup the test data
		CronExpression c = new CronExpression();

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.TRUE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageStaticExpression() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 0 0 1 1 *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("yearly"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageDayOfWeek() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 0 0 * * 1");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every Monday"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageEverySecond() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("* * * * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every second"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageEveryMinute() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 * * * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every minute"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageEveryFiveMinutes() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 */5 * * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every 5 minutes"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageMinuteFive() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 5 * * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every 5 past the hour"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageHourAmEveryMinute() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 * 10 * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every minute past 10am"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageHourPmEveryMinute() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 * 23 * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every minute past 11pm"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageHourEveryFiveMinutes() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 */5 10 * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every 5 minutes past 10am"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageHourMinuteUnderTen() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 5 10 * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every day at 10:05am"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void toNaturalLanguageHourMinute() throws Exception {
		// setup the test data
		CronExpression c = CronExpression.fromExpression("0 15 10 * * *");

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is("every day at 10:15am"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToStringAllNullReturnsEveryMinute() {
		// setup the test data
		CronExpression c = new CronExpression();

		// call the method under test
		String result = c.toString();

		// verify the result
		assertThat(result, is("0 0 0 * * *"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testToStringValidValuesReturnsCorrectExpression() {
		// setup the test data
		CronExpression c = new CronExpression(1, 2, 0, 3, null, 5);

		// call the method under test
		String result = c.toString();

		// verify the result
		assertThat(result, is("1 2 0 3 * 5"));
	}

}
