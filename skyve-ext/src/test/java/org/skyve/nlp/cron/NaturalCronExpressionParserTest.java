package org.skyve.nlp.cron;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(value = Parameterized.class)
public class NaturalCronExpressionParserTest {

	@Parameter(value = 0)
	public String expression;
	@Parameter(value = 1)
	public String expected;

	@Parameters(name = "{index}: testParse({0}) = {1}")
	public static Collection<String[]> data() {
		return Arrays.asList(new String[][] {
				{ "yearly", "0 0 0 1 1 *" },
				{ "annually", "0 0 0 1 1 *" },
				{ "monthly", "0 0 0 1 * *" },
				{ "weekly", "0 0 0 * * 0" },
				{ "daily", "0 0 0 * * *" },
				{ "midnight", "0 0 0 * * *" },
				{ "hourly", "0 0 * * * *" },
				{ "each day", "0 0 0 * * *" },
				{ "every day", "0 0 0 * * *" },
				{ "daily", "0 0 0 * * *" },
				{ "every day at 3 AM", "0 0 3 * * *" },
				{ "5am", "0 0 5 * * *" },
				{ "daily at 5am", "0 0 5 * * *" },
				{ "every friday at 5am", "0 0 5 * * 5" },
				{ "daily at 17:30", "0 30 17 * * *" },
				{ "every week", "0 0 0 * * 0" },
				{ "every minute", "0 * * * * *" },
				{ "every 5 minutes", "0 */5 * * * *" },
				{ "every 30 minutes", "0 */30 * * * *" },
				{ "every month", "0 0 0 1 * *" },
				{ "monthly", "0 0 0 1 * *" },
				{ "every Monday", "0 0 0 * * 1" },
				{ "Every monday", "0 0 0 * * 1" },
				{ "every Wednesday", "0 0 0 * * 3" },
				{ "on Fridays", "0 0 0 * * 5" },
				{ "every hour", "0 0 * * * *" },
				{ "every 6 hours", "0 0 */6 * * *" },
				{ "hourly", "0 0 * * * *" },
				{ "every year", "0 0 0 1 1 *" },
				{ "every day at 9am", "0 0 9 * * *" },
				{ "every day at 5pm", "0 0 17 * * *" },
				{ "every day at 5:45pm", "0 45 17 * * *" },
				{ "every day at 17:00", "0 0 17 * * *" },
				{ "every day at 17:25", "0 25 17 * * *" },
				{ "5:15am every Tuesday", "0 15 5 * * 2" },
				{ "7pm every Thursday", "0 0 19 * * 4" },
				{ "every May", "0 0 0 1 5 *" },
				{ "every December", "0 0 0 1 12 *" },
				{ "midnight", "0 0 0 * * *" },
				{ "midnight on tuesdays", "0 0 0 * * 2" },
				{ "every 5 minutes on Tuesdays", "0 */5 * * * 2" },
				{ "midday", "0 0 12 * * *" },
				{ "noon", "0 0 12 * * *" },
				{ "Noon", "0 0 12 * * *" },
				{ "every 25th", "0 0 0 25 * *" },
				{ "every 3rd of January", "0 0 0 3 1 *" },
				{ "every 3RD of january", "0 0 0 3 1 *" },
				{ "every 2 days", "0 0 0 */2 * *" },
				{ "every 30 days", "0 0 0 */30 * *" },
				{ "every 2 months", "0 0 0 0 */2 *" },
		});
	}

	private NaturalCronExpressionParser parser;

	@Before
	public void setup() throws Exception {
		parser = new NaturalCronExpressionParser();
	}

	/**
	 * Tests that each input expression in the parameters list matches the expected result.
	 */
	@Test
	public void testParseWithParameters() {
		// call the method under test
		CronExpression result = parser.parse(expression);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.toString(), is(expected));
	}
}
