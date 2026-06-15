package org.skyve.domain.types.converters.decimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.Decimal5;

class Decimal5TimeDurationTest {

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValuePositiveHoursAndMinutes() throws ConversionException {
		Decimal5TimeDuration converter = new Decimal5TimeDuration();
		// 1 hour 30 minutes = 1.5
		Decimal5 result = converter.fromDisplayValue("1:30");
		assertEquals(new Decimal5("1.5"), result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueZeroHoursAndMinutes() throws ConversionException {
		Decimal5TimeDuration converter = new Decimal5TimeDuration();
		Decimal5 result = converter.fromDisplayValue("0:00");
		assertEquals(Decimal5.ZERO, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueNegativeHoursSubtractsMinutes() throws ConversionException {
		Decimal5TimeDuration converter = new Decimal5TimeDuration();
		// -1 hour 30 minutes = -1.5
		Decimal5 result = converter.fromDisplayValue("-1:30");
		assertTrue(result.compareTo(Decimal5.ZERO) < 0);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueNoColonThrowsConversionException() {
		Decimal5TimeDuration converter = new Decimal5TimeDuration();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("130"));
	}

	@Test
	@SuppressWarnings("static-method")
	void fromDisplayValueInvalidNumberThrowsConversionException() {
		Decimal5TimeDuration converter = new Decimal5TimeDuration();
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("abc:30"));
	}
}
