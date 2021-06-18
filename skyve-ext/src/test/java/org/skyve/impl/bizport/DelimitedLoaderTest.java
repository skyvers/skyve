package org.skyve.impl.bizport;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.junit.Test;

public class DelimitedLoaderTest {

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringNullString() {
		// setup the test data
		String input = null;

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringEmptyString() {
		// setup the test data
		String input = "";

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringSingleQuoteOnlyReturnsEmptyString() {
		// setup the test data
		String input = "\"";

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is(""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringSingleQuoteDoesNothing() {
		// setup the test data
		String input = "\"Jessica, Freindship";

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringNoQuotesDoesNothing() {
		// setup the test data
		String input = "Jessica, Freindship";

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testRemoveQuotesSurroundingStringWithDoubleQuotes() {
		// setup the test data
		String input = "\"Jessica, Freindship\"";

		// call the method under test
		String result = DelimitedLoader.removeQuotesSurroundingString(input);

		// verify the result
		assertThat(result, is("Jessica, Freindship"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteNullString() {
		// setup the test data
		String input = null;

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteEmptyString() {
		// setup the test data
		String input = "";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteSingleQuoteDoesNothing() {
		// setup the test data
		String input = "a \"string";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteSingleQuoteOnly() {
		// setup the test data
		String input = "\"";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteNoQuotesDoesNothing() {
		// setup the test data
		String input = "a string";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is(input));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteWithDoubleQuoteRemovesQuote() {
		// setup the test data
		String input = "a string\"\"";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is("a string\""));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testReplaceEscapedDoubleQuoteWithMultipleDoubleQuoteRemovesAll() {
		// setup the test data
		String input = "a\"\" string\"\"";

		// call the method under test
		String result = DelimitedLoader.replaceEscapedDoubleQuote(input);

		// verify the result
		assertThat(result, is("a\" string\""));
	}
}
