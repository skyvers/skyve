package org.skyve.impl.web.faces.converters.integer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import jakarta.faces.convert.ConverterException;

public class IntegerConverterTest {

	// ---- IntegerConverter ----

	@Test
	void integerConverter_getAsStringReturnsEmptyForNull() {
		assertEquals("", new IntegerConverter().getAsString(null, null, null));
	}

	@Test
	void integerConverter_getAsStringFormatsValue() {
		assertNotNull(new IntegerConverter().getAsString(null, null, Integer.valueOf(42)));
	}

	@Test
	void integerConverter_getAsObjectReturnsNullForNull() {
		assertNull(new IntegerConverter().getAsObject(null, null, null));
	}

	@Test
	void integerConverter_getAsObjectParsesValidInteger() {
		assertNotNull(new IntegerConverter().getAsObject(null, null, "42"));
	}

	@Test
	void integerConverter_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new IntegerConverter().getAsObject(null, null, "not-a-number"));
	}

	// ---- IntegerSeparator ----

	@Test
	void integerSeparator_getAsStringReturnsEmptyForNull() {
		assertEquals("", new IntegerSeparator().getAsString(null, null, null));
	}

	@Test
	void integerSeparator_getAsStringFormatsValue() {
		assertNotNull(new IntegerSeparator().getAsString(null, null, Integer.valueOf(1234)));
	}

	@Test
	void integerSeparator_getAsObjectReturnsNullForNull() {
		assertNull(new IntegerSeparator().getAsObject(null, null, null));
	}

	@Test
	void integerSeparator_getAsObjectParsesRoundTrip() {
		Integer original = Integer.valueOf(1234);
		String display = new IntegerSeparator().getAsString(null, null, original);
		assertNotNull(new IntegerSeparator().getAsObject(null, null, display));
	}

	@Test
	void integerSeparator_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new IntegerSeparator().getAsObject(null, null, "not-a-number"));
	}

	// ---- LongIntegerConverter ----

	@Test
	void longIntegerConverter_getAsStringReturnsEmptyForNull() {
		assertEquals("", new LongIntegerConverter().getAsString(null, null, null));
	}

	@Test
	void longIntegerConverter_getAsStringFormatsValue() {
		assertNotNull(new LongIntegerConverter().getAsString(null, null, Long.valueOf(42L)));
	}

	@Test
	void longIntegerConverter_getAsObjectReturnsNullForNull() {
		assertNull(new LongIntegerConverter().getAsObject(null, null, null));
	}

	@Test
	void longIntegerConverter_getAsObjectParsesValidLong() {
		assertNotNull(new LongIntegerConverter().getAsObject(null, null, "42"));
	}

	@Test
	void longIntegerConverter_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new LongIntegerConverter().getAsObject(null, null, "not-a-number"));
	}

	// ---- LongIntegerSeparator ----

	@Test
	void longIntegerSeparator_getAsStringReturnsEmptyForNull() {
		assertEquals("", new LongIntegerSeparator().getAsString(null, null, null));
	}

	@Test
	void longIntegerSeparator_getAsStringFormatsValue() {
		assertNotNull(new LongIntegerSeparator().getAsString(null, null, Long.valueOf(1234L)));
	}

	@Test
	void longIntegerSeparator_getAsObjectReturnsNullForNull() {
		assertNull(new LongIntegerSeparator().getAsObject(null, null, null));
	}

	@Test
	void longIntegerSeparator_getAsObjectParsesRoundTrip() {
		Long original = Long.valueOf(1234L);
		String display = new LongIntegerSeparator().getAsString(null, null, original);
		assertNotNull(new LongIntegerSeparator().getAsObject(null, null, display));
	}

	@Test
	void longIntegerSeparator_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new LongIntegerSeparator().getAsObject(null, null, "not-a-number"));
	}

	// ---- SimplePercentage ----

	@Test
	void simplePercentage_getAsStringReturnsEmptyForNull() {
		assertEquals("", new SimplePercentage().getAsString(null, null, null));
	}

	@Test
	void simplePercentage_getAsStringFormatsValue() {
		assertNotNull(new SimplePercentage().getAsString(null, null, Integer.valueOf(50)));
	}

	@Test
	void simplePercentage_getAsObjectReturnsNullForNull() {
		assertNull(new SimplePercentage().getAsObject(null, null, null));
	}

	@Test
	void simplePercentage_getAsObjectParsesRoundTrip() {
		Integer original = Integer.valueOf(50);
		String display = new SimplePercentage().getAsString(null, null, original);
		assertNotNull(new SimplePercentage().getAsObject(null, null, display));
	}

	@Test
	void simplePercentage_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new SimplePercentage().getAsObject(null, null, "not-a-number"));
	}
}
