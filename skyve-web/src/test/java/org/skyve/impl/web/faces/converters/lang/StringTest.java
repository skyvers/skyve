package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
class StringTest {
	private String converter;

	@BeforeEach
	void before() {
		converter = new String();
	}

	@Test
	void testGetAsObjectEmptyStringValueReturnsNull() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, ""), is(nullValue()));
	}

	@Test
	void testGetAsObjectWhitespaceStringValueReturnsNull() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, " "), is(nullValue()));
	}

	@Test
	void testGetAsObjectWordValueTrimsWhitespace() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, " word "), is("word"));
	}

	@Test
	void testGetAsObjectWordValue() {
		// setup the test data
		java.lang.String testValue = "word";

		// call the method under test
		assertThat(converter.getAsObject(null, null, "word"), is(testValue));
	}


	@Test
	void testGetAsStringTrueValue() {
		// setup the test data
		java.lang.String testValue = "test";

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("test"));
	}
}
