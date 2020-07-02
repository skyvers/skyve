package org.skyve.domain.types.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

import java.text.ParseException;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

public class DD_MMM_YYYYTest {

	private DD_MMM_YYYY formatter;

	@Before
	public void before() {
		formatter = new DD_MMM_YYYY();
	}

	@Test(expected = ParseException.class)
	public void testFromDisplayValueInvalidFormat() throws Exception {
		// call the method under test
		formatter.fromDisplayValue("03-01-2020");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testFromDisplayValue() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.fromDisplayValue("01-Mar-2020"), is(testDate));
	}

	@Test
	public void testToDisplayValue() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01-Mar-2020"));
	}
}
