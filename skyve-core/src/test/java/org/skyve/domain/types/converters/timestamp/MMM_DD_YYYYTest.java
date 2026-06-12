package org.skyve.domain.types.converters.timestamp;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Time;

class MMM_DD_YYYYTest {

	private MMM_DD_YYYY formatter;

	@BeforeEach
	void before() {
		formatter = new MMM_DD_YYYY();
	}

	@Test
	void testFromDisplayValueInvalidFormat() {
		ConversionException ce = assertThrows(ConversionException.class, () -> formatter.fromDisplayValue("03-01-2020"));

		assertTrue(ce.getMessages().size() > 0);
	}

	@Test
	void testFromDisplayValue() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.fromDisplayValue("Mar-01-2020"), is(testDate));
	}

	@Test
	void testToDisplayValue() {
		// setup the test data
		Timestamp testDate = new Timestamp(Time.withDate(01, 03, 2020));

		// call the method under test
		assertThat(formatter.toDisplayValue(testDate), is("Mar-01-2020"));
	}
}
