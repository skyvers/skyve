package org.skyve.domain.types.converters.datetime;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateTime;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Time;

public class DD_MM_YYYY_HH_MITest {

	private DD_MM_YYYY_HH_MI formatter;

	@BeforeEach
	public void before() {
		formatter = new DD_MM_YYYY_HH_MI();
	}

	@Test
	public void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// setup the test data
			DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
			Time.setTime(testDate, 02, 30, 0);

			// call the method under test
			assertThat(formatter.fromDisplayValue("01-03-2020 02:30"), is(testDate));
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	public void testFromDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30 AM"), is(testDate));
	}

	@Test
	public void testFromDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30 PM"), is(testDate));
	}

	@Test
	public void testToDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30 AM"));
	}

	@Test
	public void testToDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30 PM"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetValueTypeIsDateTime() {
		assertEquals(DateTime.class, new DD_MM_YYYY_HH_MI().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetFormatIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI().getFormat(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetValidatorIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI().getValidator(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGetAttributeTypeIsDateTime() {
		assertThat(new DD_MM_YYYY_HH_MI().getAttributeType(), is(AttributeType.dateTime));
	}

	@Test
	@SuppressWarnings({ "static-method", "null" })
	public void testToDisplayValueNullThrows() {
		assertThrows(ConversionException.class, () -> new DD_MM_YYYY_HH_MI().toDisplayValue(null));
	}
}
