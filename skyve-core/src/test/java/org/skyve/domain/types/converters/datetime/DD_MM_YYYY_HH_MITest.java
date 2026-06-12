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

class DD_MM_YYYY_HH_MITest {

	private DD_MM_YYYY_HH_MI formatter;

	@BeforeEach
	void before() {
		formatter = new DD_MM_YYYY_HH_MI();
	}

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> formatter.fromDisplayValue("01-03-2020 02:30"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30 AM"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30 PM"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30 AM"));
	}

	@Test
	void testToDisplayValuePM() throws Exception {
		// setup the test data
		DateTime testDate = new DateTime(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 0);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30 PM"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValueTypeIsDateTime() {
		assertEquals(DateTime.class, new DD_MM_YYYY_HH_MI().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFormatIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI().getFormat(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValidatorIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI().getValidator(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributeTypeIsDateTime() {
		assertThat(new DD_MM_YYYY_HH_MI().getAttributeType(), is(AttributeType.dateTime));
	}

	@Test
	@SuppressWarnings({ "static-method", "null" })
	void testToDisplayValueNullThrows() {
		DD_MM_YYYY_HH_MI converter = new DD_MM_YYYY_HH_MI();
		assertThrows(ConversionException.class, () -> converter.toDisplayValue(null));
	}
}
