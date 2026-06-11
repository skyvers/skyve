package org.skyve.impl.web.faces.converters.decimal;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;

import jakarta.faces.convert.ConverterException;
public class Decimal5TimeDurationTest {
	private Decimal5TimeDuration converter;

	@Before
	public void before() {
		converter = new Decimal5TimeDuration();
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
		converter.getAsObject(null, null, "not a duration");
		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidValue() {
		// "1:30" represents 1 hour 30 minutes = 1.5
		org.skyve.domain.types.Decimal5 expected = new org.skyve.domain.types.Decimal5("1.5");
		assertThat(converter.getAsObject(null, null, "1:30"), is(expected));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(converter.getAsString(null, null, null), is(""));
	}

	@Test
	public void testGetAsStringValidValue() {
		org.skyve.domain.types.Decimal5 value = new org.skyve.domain.types.Decimal5("1.5");
		assertThat(converter.getAsString(null, null, value), is("1:30"));
	}
}
