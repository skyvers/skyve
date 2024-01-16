package org.skyve.domain.types.converters.date;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateOnly;
import org.skyve.util.Time;

public class MM_DD_YYYYTest {

	private MM_DD_YYYY formatter;

	@Before
	public void before() {
		formatter = new MM_DD_YYYY();
	}

	@Test(expected = ConversionException.class)
	public void testFromDisplayValueInvalidFormat() throws Exception {
		// call the method under test
		formatter.fromDisplayValue("03-01-2020");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testFromDisplayValueValidFormat() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		assertThat(formatter.fromDisplayValue("03/01/2020"), is(testDate));
	}

	@Test
	public void testToDisplayValue() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("03/01/2020"));
	}
}
