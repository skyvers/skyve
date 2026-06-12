package org.skyve.domain.types.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Time;

class DD_MM_YYYY_HH_MI_SSTest {

	private DD_MM_YYYY_HH_MI_SS formatter;

	@BeforeEach
	void before() {
		formatter = new DD_MM_YYYY_HH_MI_SS();
	}

	@Test
	void testFromDisplayValueInvalidFormat() {
		ConversionException ce = assertThrows(ConversionException.class, () -> formatter.fromDisplayValue("01-03-2020 02:30:05"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30:55 AM"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020 02:30:55 PM"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30:55 AM"));
	}

	@Test
	void testToDisplayValuePM() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020 02:30:55 PM"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValueTypeIsTimestamp() {
		assertEquals(Timestamp.class, new DD_MM_YYYY_HH_MI_SS().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFormatIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI_SS().getFormat(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValidatorIsNull() {
		assertThat(new DD_MM_YYYY_HH_MI_SS().getValidator(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributeTypeIsTimestamp() {
		assertThat(new DD_MM_YYYY_HH_MI_SS().getAttributeType(), is(AttributeType.timestamp));
	}

	@Test
	@SuppressWarnings({ "static-method", "null" })
	void testToDisplayValueNullThrows() {
		DD_MM_YYYY_HH_MI_SS converter = new DD_MM_YYYY_HH_MI_SS();
		assertThrows(ConversionException.class, () -> converter.toDisplayValue(null));
	}
}
