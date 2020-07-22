package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;

public class StringTest {

	private String converter;

	@Before
	public void before() {
		converter = new String();
	}

	@Test
	public void testGetAsObjectEmptyStringValueReturnsNull() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, ""), is(nullValue()));
	}

	@Test
	public void testGetAsObjectWhitespaceStringValueReturnsNull() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, " "), is(nullValue()));
	}

	@Test
	public void testGetAsObjectWordValueTrimsWhitespace() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, " word "), is("word"));
	}

	@Test
	public void testGetAsObjectWordValue() throws Exception {
		// setup the test data
		java.lang.String testValue = "word";

		// call the method under test
		assertThat(converter.getAsObject(null, null, "word"), is(testValue));
	}


	@Test
	public void testGetAsStringTrueValue() throws Exception {
		// setup the test data
		java.lang.String testValue = "test";

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("test"));
	}
}
