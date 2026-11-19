package org.skyve.impl.web.faces.converters.lang;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
class BooleanTest {

	private Boolean converter;

	@BeforeEach
	void before() {
		converter = new Boolean();
	}

	@Test
	void testGetAsObjectInvalidStringValueReturnsFalse() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "not a boolean"), is(java.lang.Boolean.FALSE));
	}

	@Test
	void testGetAsObjectUppercaseFalseValue() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "FALSE"), is(java.lang.Boolean.FALSE));
	}

	@Test
	void testGetAsObjectUppercaseTrueValue() {
		// call the method under test
		assertThat(converter.getAsObject(null, null, "TRUE"), is(java.lang.Boolean.TRUE));
	}

	@Test
	void testGetAsObjectValidFalseValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "false"), is(testValue));
	}

	@Test
	void testGetAsObjectValidTrueValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsObject(null, null, "true"), is(testValue));
	}

	@Test
	void testGetAsStringFalseValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.FALSE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("false"));
	}

	@Test
	void testGetAsStringTrueValue() {
		// setup the test data
		java.lang.Boolean testValue = java.lang.Boolean.TRUE;

		// call the method under test
		assertThat(converter.getAsString(null, null, testValue), is("true"));
	}
}
