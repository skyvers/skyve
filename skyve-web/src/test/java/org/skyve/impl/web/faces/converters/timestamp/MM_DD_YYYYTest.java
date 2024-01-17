package org.skyve.impl.web.faces.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import javax.faces.convert.ConverterException;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

public class MM_DD_YYYYTest {
	private MM_DD_YYYY formatter;

	@Before
	public void before() {
		formatter = new MM_DD_YYYY();
	}

	@Test(expected = ConverterException.class)
	public void testGetAsObjectInvalidFormat() throws Exception {
		// call the method under test
		formatter.getAsObject(null, null, "03-01-2020");

		fail("Should throw exception before this line.");
	}

	@Test
	public void testGetAsObjectValidFormat() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.getAsObject(null, null, "03/01/2020"), is(testDate));
	}

	@Test
	public void testGetAsString() throws Exception {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.getAsString(null, null, testDate), is("03/01/2020"));
	}
}
