package org.skyve.domain.types.converters.date;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.util.Time;

class DD_MM_YYYYTest {

	private DD_MM_YYYY formatter;

	@BeforeEach
	void before() {
		formatter = new DD_MM_YYYY();
	}

	@Test
	void testFromDisplayValueInvalidFormat() throws Exception {
		ConversionException ce = assertThrows(ConversionException.class, () -> formatter.fromDisplayValue("03-01-2020"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValueValidFormat() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		assertThat(formatter.fromDisplayValue("01/03/2020"), is(testDate));
	}

	@Test
	void testToDisplayValue() throws Exception {
		// setup the test data
		DateOnly testDate = Time.withDate(01, 03, 2020);

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("01/03/2020"));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValueTypeIsDateOnly() {
		assertEquals(DateOnly.class, new DD_MM_YYYY().getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetFormatIsNull() {
		assertThat(new DD_MM_YYYY().getFormat(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetValidatorIsNull() {
		assertThat(new DD_MM_YYYY().getValidator(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGetAttributeTypeIsDate() {
		assertThat(new DD_MM_YYYY().getAttributeType(), is(AttributeType.date));
	}

	@Test
	@SuppressWarnings({ "static-method", "null" })
	void testToDisplayValueNullThrows() {
		DD_MM_YYYY converter = new DD_MM_YYYY();
		assertThrows(ConversionException.class, () -> converter.toDisplayValue(null));
	}
}
