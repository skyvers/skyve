package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;

public class BooleanTest {

	private Boolean converter;

	@Before
	public void before() {
		converter = new Boolean();
	}

	@Test
	public void testGetAsObjectInvalidStringValueReturnsFalse() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "not a boolean"), is(java.lang.Boolean.FALSE));
	}

	@Test
	public void testGetAsObjectUppercaseFalseValue() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "FALSE"), is(java.lang.Boolean.FALSE));
	}

	@Test
	public void testGetAsObjectUppercaseTrueValue() throws Exception {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "TRUE"), is(java.lang.Boolean.TRUE));
	}

	@Test
	public void testGetAsObjectValidFalseValue() throws Exception {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "false"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidTrueValue() throws Exception {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "true"), is(testValue));
	}

	@Test
	public void testGetAsStringFalseValue() throws Exception {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("false"));
	}

	@Test
	public void testGetAsStringTrueValue() throws Exception {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("true"));
	}
}
