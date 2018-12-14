package org.skyve.impl.bind;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.Test;

public class BindUtilTest {

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierLeavesValidIndentifier() {
		// setup the test data
		String identifier = "validIdentifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is(identifier));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("whitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaInstanceIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaInstanceIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesInvalidCharacters() {
		// setup the test data
		String identifier = "E-Mail";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("EMail"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("WhitespaceIdentifier"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaTypeIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaTypeIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierRemovesWhitespace() {
		// setup the test data
		String identifier = "Whitespace Identifier";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("WHITESPACE_IDENTIFIER"));
	}

	/**
	 * Two leading capitals shouldn't have any effect on the static identifier, this
	 * just makes sure the Introspector.decapitalize has no negative effects.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testToJavaStaticIdentifierPreservesTwoLeadingCapitals() {
		// setup the test data
		String identifier = "DOB";

		// call the method under test
		String result = BindUtil.toJavaStaticIdentifier(identifier);

		// verify the result
		assertThat(result, is("DOB"));
	}
}
