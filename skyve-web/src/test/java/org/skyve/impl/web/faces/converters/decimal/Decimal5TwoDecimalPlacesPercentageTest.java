package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
public class Decimal5TwoDecimalPlacesPercentageTest {
	private Decimal5TwoDecimalPlacesPercentage converter;

	@Before
	public void before() {
		converter = new Decimal5TwoDecimalPlacesPercentage();
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
		// "50%" represents 50% i.e. 0.50 as a Decimal5
		org.skyve.domain.types.Decimal5 result = converter.getAsObject(null, null, "50%");
		assertThat(converter.getAsString(null, null, result), is("50.00%"));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() {
		// 0.5 Decimal5 → 50.00%
		org.skyve.domain.types.Decimal5 value = new org.skyve.domain.types.Decimal5("0.5");
		assertThat(converter.getAsString(null, null, value), is("50.00%"));
	}
}
