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
	public void testGetAsObjectInvalidStringValueReturnsFalse() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "not a boolean"), is(java.lang.Boolean.FALSE));
	}

	@Test
	public void testGetAsObjectUppercaseFalseValue() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "FALSE"), is(java.lang.Boolean.FALSE));
	}

	@Test
	public void testGetAsObjectUppercaseTrueValue() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "TRUE"), is(java.lang.Boolean.TRUE));
	}

	@Test
	public void testGetAsObjectValidFalseValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "false"), is(testValue));
	}

	@Test
	public void testGetAsObjectValidTrueValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "true"), is(testValue));
	}

	@Test
	public void testGetAsStringFalseValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("false"));
	}

	@Test
	public void testGetAsStringTrueValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("true"));
	}
}
