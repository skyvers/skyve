package org.skyve.util.test;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

public class TestUtilTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexAllowedDigitCount() {
		// setup the test data
		final String expression = "(P|AH){1}\\d{5}";

		// call the method under test
		String result = TestUtil.randomRegex(expression, Integer.valueOf(10));

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, anyOf(startsWith("P"), startsWith("AH")));
		assertThat(result.matches("(P|AH){1}\\d{5}"), is(true));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRandomRegexListOfAllowedValues() {
		// setup the test data
		final String expression = "(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)";

		// call the method under test
		String result = TestUtil.randomRegex(expression, Integer.valueOf(3));

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("Should generate a value that is in the list",
				Arrays.asList("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), hasItem(result));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRandomRegexListOfAllowedValuesWithAnchors() {
		// setup the test data
		final String expression = "^(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, Integer.valueOf(3));

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("Should generate a value that is in the list",
				Arrays.asList("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), hasItem(result));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexLowercaseDashSeparatedString() {
		// setup the test data
		final String expression = "[a-z0-9]+(-[a-z0-9]+)*";

		// call the method under test
		String result = TestUtil.randomRegex(expression, Integer.valueOf(20));

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.matches("[a-z0-9]+(-[a-z0-9]+)*"), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexLowercaseDashSeparatedStringWithAnchors() {
		// setup the test data
		final String expression = "^[a-z0-9]+(-[a-z0-9]+)*$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, Integer.valueOf(20));

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.matches("[a-z0-9]+(-[a-z0-9]+)*"), is(true));
	}
}
