package org.skyve.impl.util;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThanOrEqualTo;

import java.util.Arrays;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.util.test.TestUtil;

public class TestUtilTest {

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexAllowedDigitCount() {
		// setup the test data
		final String expression = "(P|AH){1}\\d{5}";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 10);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, anyOf(startsWith("P"), startsWith("AH")));
		assertThat(result.matches("(P|AH){1}\\d{5}"), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexListOfAllowedValues() {
		// setup the test data
		final String expression = "(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 3);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat("Should generate a value that is in the list",
				Arrays.asList("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), hasItem(result));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexListOfAllowedValuesWithAnchors() {
		// setup the test data
		final String expression = "^(ACT|NSW|NT|QLD|SA|TAS|VIC|WA)$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 3);

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
		String result = TestUtil.randomRegex(expression, 20);

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
		String result = TestUtil.randomRegex(expression, 20);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.matches("[a-z0-9]+(-[a-z0-9]+)*"), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomRegexStripsAnchors() {
		// setup the test data
		final String expression = "^^\\$$";

		// call the method under test
		String result = TestUtil.randomRegex(expression, 10);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result, is("^$"));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomEmail() {
		// setup the test data
		final int length = 20;

		// call the method under test
		String result = TestUtil.randomEmail(length);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.length(), is(length));
		assertThat(result.matches("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"), is(true));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomDecimal() {
		// setup the test data
		Decimal10 decimal10 = new Decimal10();
		Decimal5 decimal5 = new Decimal5();
		Decimal2 decimal2 = new Decimal2();

		// call the method under test
		Decimal result10 = TestUtil.randomDecimal(decimal10);
		Decimal result5 = TestUtil.randomDecimal(decimal5);
		Decimal result2 = TestUtil.randomDecimal(decimal2);

		// verify the results
		assertThat(result10, is(notNullValue()));
		assertThat(result5, is(notNullValue()));
		assertThat(result2, is(notNullValue()));
		
		// Check decimal places
		assertThat(result10.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(10)));
		assertThat(result5.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(5)));
		assertThat(result2.toString().split("\\.")[1].length(), is(lessThanOrEqualTo(2)));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomInteger() {
		// setup the test data
		Integer integer = new Integer();
		LongInteger longInteger = new LongInteger();
		
		// Set up validators
		IntegerValidator intValidator = new IntegerValidator();
		intValidator.setMin(0);
		intValidator.setMax(100);
		integer.setValidator(intValidator);
		
		LongValidator longValidator = new LongValidator();
		longValidator.setMin(0L);
		longValidator.setMax(1000L);
		longInteger.setValidator(longValidator);

		// call the method under test
		java.lang.Integer resultInt = TestUtil.randomInteger(integer);
		java.lang.Integer resultLong = TestUtil.randomInteger(longInteger);

		// verify the results
		assertThat(resultInt, is(notNullValue()));
		assertThat(resultLong, is(notNullValue()));
		assertThat(resultInt, is(greaterThanOrEqualTo(0)));
		assertThat(resultInt, is(lessThanOrEqualTo(100)));
		assertThat(resultLong, is(greaterThanOrEqualTo(0)));
		assertThat(resultLong, is(lessThanOrEqualTo(1000)));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void testRandomEnum() {
		// setup the test data
		enum TestEnum { A, B, C, D }
		Class<TestEnum> enumClass = TestEnum.class;

		// call the method under test
		TestEnum result1 = TestUtil.randomEnum(enumClass, null);
		TestEnum result2 = TestUtil.randomEnum(enumClass, 1); // B

		// verify the results
		assertThat(result1, is(notNullValue()));
		assertThat(result2, is(notNullValue()));
		assertThat(Arrays.asList(TestEnum.values()), hasItem(result1));
		assertThat(Arrays.asList(TestEnum.values()), hasItem(result2));
		assertThat(result2, is(not(TestEnum.B))); // Should not return the current value
	}
}
