package org.skyve.impl.web.faces.converters.decimal.currency;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
class Decimal2DollarsAndCentsTest {
	private Decimal2DollarsAndCents converter;

	@Before
	public void before() {
		converter = new Decimal2DollarsAndCents();
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
		org.skyve.domain.types.Decimal2 expected = new org.skyve.domain.types.Decimal2("3.14");
		assertThat(converter.getAsObject(null, null, "3.14"), is(expected));
	}

	@Test
	public void testGetAsStringNullValue() throws Exception {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() throws Exception {
		org.skyve.domain.types.Decimal2 value = org.skyve.domain.types.Decimal2.ONE_HUNDRED;
		assertThat(converter.getAsString(null, null, value), is("100.00"));
	}
}
