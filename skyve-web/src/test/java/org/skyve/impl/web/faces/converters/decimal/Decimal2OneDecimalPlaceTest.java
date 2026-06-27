package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
public class Decimal2OneDecimalPlaceTest {
	private Decimal2OneDecimalPlace converter;

	@Before
	public void before() {
		converter = new Decimal2OneDecimalPlace();
	}

	@Test
	public void testGetAsObjectNullValue() {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	public void testGetAsObjectEmptyValue() {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidValue() {
		converter.getAsObject(null, null, "not a decimal");
		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidValue() {
		org.skyve.domain.types.Decimal2 expected = new org.skyve.domain.types.Decimal2("3.1");
		assertThat(converter.getAsObject(null, null, "3.1"), is(expected));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() {
		org.skyve.domain.types.Decimal2 value = org.skyve.domain.types.Decimal2.ONE_HUNDRED;
		assertThat(converter.getAsString(null, null, value), is("100.0"));
	}
}
