package org.skyve.domain.types.converters.time;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.TimeOnly;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Time;

class HH_MITest {

	private HH_MI formatter;

	@BeforeEach
	void before() {
		formatter = new HH_MI();
	}

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> {
			// setup the test data
			TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
			Time.setTime(testDate, 02, 30, 05);

			// call the method under test
			assertThat(formatter.fromDisplayValue("02:30:05"), is(testDate));
		});

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30 AM"), is(testDate));
	}

	@Test
	void testFromDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 00);

		// call the method under test
		assertThat(formatter.fromDisplayValue("02:30 PM"), is(testDate));
	}

	@Test
	void testToDisplayValueAM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 02, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30 AM"));
	}

	@Test
	void testToDisplayValuePM() throws Exception {
		// setup the test data
		TimeOnly testDate = new TimeOnly(Time.withDate(01, 03, 2020));
		Time.setTime(testDate, 14, 30, 55);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("02:30 PM"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValueTypeIsTimeOnly() {
		assertEquals(TimeOnly.class, new HH_MI().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFormatIsNull() {
		assertThat(new HH_MI().getFormat(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValidatorIsNull() {
		assertThat(new HH_MI().getValidator(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributeTypeIsTime() {
		assertThat(new HH_MI().getAttributeType(), is(AttributeType.time));
	}

	@Test
	@SuppressWarnings({ "static-method", "null" })
	void testToDisplayValueNullThrows() {
		assertThrows(ConversionException.class, () -> new HH_MI().toDisplayValue(null));
	}
}
