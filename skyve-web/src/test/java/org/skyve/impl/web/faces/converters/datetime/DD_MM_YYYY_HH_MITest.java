package org.skyve.impl.web.faces.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

public class DD_MM_YYYY_HH_MITest {

	private DD_MM_YYYY_HH_MI formatter;

	@Before
	public void before() {
		formatter = new DD_MM_YYYY_HH_MI();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "03-01-2020 02:30");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormatAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "01/03/2020 02:30 AM"), is(testDate));
	}

	@Test
	public void testGetAsObjectValidFormatPM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "01/03/2020 02:30 PM"), is(testDate));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("01/03/2020 02:30 AM"));
	}
}
