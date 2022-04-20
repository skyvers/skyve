package org.skyve.domain.types.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.util.Time;

public class MMM_DD_YYYYTest {

	private MMM_DD_YYYY formatter;

	@Before
	public void before() {
		formatter = new MMM_DD_YYYY();
	}

	@Test(expected = ConversionException.class)
	public void testFromDisplayValueInvalidFormat() throws Exception {
		// call the method under test
		formatter.fromDisplayValue("03-01-2020");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testFromDisplayValue() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.fromDisplayValue("Mar-01-2020"), is(testDate));
	}

	@Test
	public void testToDisplayValue() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("Mar-01-2020"));
	}
}
