package org.skyve.nlp.cron;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
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
	public void toNaturalLanguageFormatsExpressions() throws Exception {
		String[][] cases = {
				{"0 0 0 1 1 *", "yearly"},
				{"0 0 0 * * 1", "every Monday"},
				{"* * * * * *", "every second"},
				{"0 * * * * *", "every minute"},
				{"0 */5 * * * *", "every 5 minutes"},
				{"0 5 * * * *", "every 5 past the hour"},
				{"0 * 10 * * *", "every minute past 10am"},
				{"0 * 23 * * *", "every minute past 11pm"},
				{"0 */5 10 * * *", "every 5 minutes past 10am"},
				{"0 5 10 * * *", "every day at 10:05am"},
				{"0 15 10 * * *", "every day at 10:15am"}};

		for (String[] testCase : cases) {
			CronExpression c = CronExpression.fromExpression(testCase[0]);

			assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));
			assertThat(c.toNaturalLanguage(), is(testCase[1]));
		}
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

	@Test
	@SuppressWarnings("static-method")
	public void testToNaturalLanguageDayNumberSuffixes() {
		String[][] cases = {
				{"0 0 0 2 * *", "every 2nd"},
				{"0 0 0 3 * *", "every 3rd"},
				{"0 0 0 12 * *", "every 12th"}};

		for (String[] testCase : cases) {
			CronExpression c = CronExpression.fromExpression(testCase[0]);
			assertThat(c.toNaturalLanguage(), is(testCase[1]));
		}
	}

        @Test
        @SuppressWarnings("static-method")
        public void testIntegerConstructorWithNullsStoresNulls() {
                // covers null branch in Integer constructor at L35 and L38
                CronExpression c = new CronExpression((Integer) null, (Integer) null, (Integer) null, (Integer) null, (Integer) null, (Integer) null);
                assertThat(c.getSecond(), is(nullValue()));
                assertThat(c.getMinute(), is(nullValue()));
                assertThat(c.getHour(), is(nullValue()));
                assertThat(c.getDayNumber(), is(nullValue()));
                assertThat(c.getMonth(), is(nullValue()));
                assertThat(c.getDayOfWeek(), is(nullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        public void testToNaturalLanguagePmHourWithStepMinute() {
                // covers L229-230: timeStepSetPattern matches for minute, hour >= 12 (PM)
                CronExpression c = CronExpression.fromExpression("0 */5 14 * * *");
                String result = c.toNaturalLanguage();
                assertThat(result, is(org.hamcrest.CoreMatchers.notNullValue()));
        }

        @Test
        @SuppressWarnings("static-method")
        public void testToNaturalLanguagePmHourWithZeroMinute() {
                // covers L244-245: minute = "0", hour >= 12 (PM)
                CronExpression c = CronExpression.fromExpression("0 0 15 * * *");
                String result = c.toNaturalLanguage();
                assertThat(result, is(org.hamcrest.CoreMatchers.notNullValue()));
        }

	@Test
	@SuppressWarnings("static-method")
	public void testConstructorWithNonNullMonthCoversL38TrueBranch() {
		// Call the Integer constructor with a non-null month to cover
		// the String.valueOf(month) branch of the ternary on that line.
		CronExpression c = new CronExpression(null, null, null, null, Integer.valueOf(6), null);
		assertThat(c.getMonth(), is("6"));
		assertThat(c.getSecond(), is(nullValue()));
	}

}
