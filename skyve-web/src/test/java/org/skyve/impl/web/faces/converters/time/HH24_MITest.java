package org.skyve.impl.web.faces.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.TimeOnly;
import org.skyve.util.Time;

import jakarta.faces.convert.ConverterException;
public class HH24_MITest {
	private HH24_MI formatter;

	@Before
	public void before() {
		formatter = new HH24_MI();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() {
		// call the method under test
		formatter.getAsObject(null, null, "03-01-2020 02:30:05");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 00);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "02:30"), is(testDate));
	}

	@Test
	public void testGetAsObjectValidFormatPM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 00);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "14:30"), is(testDate));
	}

	@Test
	public void testGetAsStringAM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("02:30"));
	}

	@Test
	public void testGetAsStringPM() {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("14:30"));
	}

	@Test
	public void testGetAsObjectNullValue() {
		assertNull(formatter.getAsObject(null, null, null));
	}

	@Test
	public void testGetAsStringNullValue() {
		assertThat(formatter.getAsString(null, null, null), is(""));
	}
}