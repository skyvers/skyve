package org.skyve.nlp.cron;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class CronExpressionParameterisedTest {

	@Parameter(value = 0)
	public String expression;
	@Parameter(value = 1)
	public String expected;

	private NaturalCronExpressionParser parser;

	@Parameters(name = "{index}: testParse({0}) = {1}")
	public static Collection<String[]> data() {
		return Arrays.asList(new String[][] {
				{ "0 0 0 1 1 *", "yearly" },
				{ "0 0 0 1 * *", "monthly" },
				{ "0 0 0 * * 0", "weekly" },
				{ "0 0 0 * * *", "daily" },
				{ "0 0 * * * *", "hourly" },
				{ "0 0 12 * * *", "midday" },
				{ "0 0 3 * * *", "every day at 3am" },
				{ "0 0 0 * * 1", "every Monday" },
				{ "0 * * * * *", "every minute" },
				{ "0 */5 * * * *", "every 5 minutes" },
				// { "0 5 * * * *", "every 5 past the hour" },
				{ "0 * 10 * * *", "every minute past 10am" },
				{ "0 * 23 * * *", "every minute past 11pm" },
				{ "0 */5 10 * * *", "every 5 minutes past 10am" },
				{ "0 30 17 * * *", "every day at 5:30pm" },
				{ "0 5 10 * * *", "every day at 10:05am" },
				{ "0 15 20 * * *", "every day at 8:15pm" },
				{ "0 0 */6 * * *", "every 6 hours" },
				{ "0 */5 * * * 2", "every 5 minutes on Tuesdays" },
				{ "0 0 0 25 * *", "every 25th" },
				{ "0 0 0 31 1 *", "every 31st of January" },
				// { "0 * * 31 1 *", "every minute on the 31st of January" },
				// { "0 * * * 1 *", "every minute in January" },
				// { "0 0 * * 1 *", "every hour in January" },
				// { "0 0 0 */30 * *", "every 30 days" },
				// { "0 0 0 0 */2 *", "every 2 months" },
				// { "0 10 1 * * 2,3,4,5,6", "" },
				// { "0 30 23 * * 2,3,4,5,6", "" },
				// { "0 0 1 * * *", "every day at 1am" },
				/*{ "0 0 7,8,9,10,11,12,13,14,15,16,17,18 * * 2,3,4,5,6", "" },
				{ "0 15 23 * * 2,3,4,5,6", "" },
				{ "0 30 2 * * 2,3,4,5,6", "" },
				{ "0 5 18 * * 2,3,4,5,6", "" },
				{ "0 0 23 * * 2,3,4,5,6", "" },
				{ "0 30 8 * * *", "" },
				{ "0 5 1 * * *", "" },
				{ "0 0 3 L 3,6,9,12 *", "" },
				{ "0 20 2 * * 2,3,4,5,6", "" },
				{ "0 25 7 * * 2,3,4,5,6", "" },
				{ "0 0 2 * * 2,3,4,5,6", "" },
				{ "0 40 1 * * 2,3,4,5,6", "" },
				{ "0 45 5 1 * *", "" },
				{ "0 45 23 * * *", "" },
				{ "0 30 18 * * 2,3,4,5,6", "" },
				{ "0 5 0 * * 2,3,4,5,6", "" },
				{ "0 0 8 * * *", "" },
				{ "0 0 6 * * 2", "" },*/
		});
	}

	@Before
	public void setup() throws Exception {
		parser = new NaturalCronExpressionParser();
	}

	/**
	 * Tests that each input expression in the parameters list matches the expected result.
	 */
	@Test
	public void testToNaturalLanguageWithParameters() {
		// setup the test data
		CronExpression c = CronExpression.fromExpression(expression);

		// validate the test data
		assertThat(Boolean.valueOf(c.hasNothing()), is(Boolean.FALSE));

		// call the method under test
		String result = c.toNaturalLanguage();

		// verify the result
		assertThat(result, is(expected));

		// parse the expression back in
		CronExpression cron = parser.parse(expected);

		// verify the resulting cron matches the original expression
		assertThat(cron.toString(), is(expression));
	}
}
