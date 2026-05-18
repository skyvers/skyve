package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;

public class Decimal5OneDecimalPlaceTest {
	private Decimal5OneDecimalPlace converter;

	@Before
	public void before() {
		converter = new Decimal5OneDecimalPlace();
	}

	@Test
	public void testGetAsObjectNullValue() throws Exception {
		assertThat(converter.getAsObject(null, null, null), nullValue());
	}

	@Test
	public void testGetAsObjectEmptyValue() throws Exception {
		assertThat(converter.getAsObject(null, null, ""), nullValue());
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidValue() throws Exception {
		converter.getAsObject(null, null, "not a decimal");
		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidValue() throws Exception {
		org.skyve.domain.types.Decimal5 expected = new org.skyve.domain.types.Decimal5("3.1");
		assertThat(converter.getAsObject(null, null, "3.1"), is(expected));
	}

	@Test
	public void testGetAsStringNullValue() throws Exception {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() throws Exception {
		org.skyve.domain.types.Decimal5 value = org.skyve.domain.types.Decimal5.ONE_HUNDRED;
		assertThat(converter.getAsString(null, null, value), is("100.0"));
	}
}
