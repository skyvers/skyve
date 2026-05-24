package org.skyve.impl.web.faces.converters.decimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;

import jakarta.faces.convert.ConverterException;

public class DecimalConverterTest {

	// ---- Decimal2Converter ----

	@Test
	void decimal2Converter_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2Converter().getAsString(null, null, null));
	}

	@Test
	void decimal2Converter_getAsStringFormatsValue() {
		assertNotNull(new Decimal2Converter().getAsString(null, null, new Decimal2("1.23")));
	}

	@Test
	void decimal2Converter_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2Converter().getAsObject(null, null, null));
	}

	@Test
	void decimal2Converter_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("1.23");
		String display = new Decimal2Converter().getAsString(null, null, original);
		assertNotNull(new Decimal2Converter().getAsObject(null, null, display));
	}

	@Test
	void decimal2Converter_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2Converter().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal2Integer ----

	@Test
	void decimal2Integer_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2Integer().getAsString(null, null, null));
	}

	@Test
	void decimal2Integer_getAsStringFormatsValue() {
		assertNotNull(new Decimal2Integer().getAsString(null, null, new Decimal2("1234")));
	}

	@Test
	void decimal2Integer_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2Integer().getAsObject(null, null, null));
	}

	@Test
	void decimal2Integer_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("1234");
		String display = new Decimal2Integer().getAsString(null, null, original);
		assertNotNull(new Decimal2Integer().getAsObject(null, null, display));
	}

	@Test
	void decimal2Integer_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2Integer().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal2IntegerPercentage ----

	@Test
	void decimal2IntegerPercentage_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2IntegerPercentage().getAsString(null, null, null));
	}

	@Test
	void decimal2IntegerPercentage_getAsStringFormatsValue() {
		assertNotNull(new Decimal2IntegerPercentage().getAsString(null, null, new Decimal2("50")));
	}

	@Test
	void decimal2IntegerPercentage_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2IntegerPercentage().getAsObject(null, null, null));
	}

	@Test
	void decimal2IntegerPercentage_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("50");
		String display = new Decimal2IntegerPercentage().getAsString(null, null, original);
		assertNotNull(new Decimal2IntegerPercentage().getAsObject(null, null, display));
	}

	@Test
	void decimal2IntegerPercentage_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2IntegerPercentage().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal2OneDecimalPlace ----

	@Test
	void decimal2OneDecimalPlace_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2OneDecimalPlace().getAsString(null, null, null));
	}

	@Test
	void decimal2OneDecimalPlace_getAsStringFormatsValue() {
		assertNotNull(new Decimal2OneDecimalPlace().getAsString(null, null, new Decimal2("1.5")));
	}

	@Test
	void decimal2OneDecimalPlace_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2OneDecimalPlace().getAsObject(null, null, null));
	}

	@Test
	void decimal2OneDecimalPlace_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("1.5");
		String display = new Decimal2OneDecimalPlace().getAsString(null, null, original);
		assertNotNull(new Decimal2OneDecimalPlace().getAsObject(null, null, display));
	}

	@Test
	void decimal2OneDecimalPlace_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2OneDecimalPlace().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5Converter ----

	@Test
	void decimal5Converter_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5Converter().getAsString(null, null, null));
	}

	@Test
	void decimal5Converter_getAsStringFormatsValue() {
		assertNotNull(new Decimal5Converter().getAsString(null, null, new Decimal5("1.23456")));
	}

	@Test
	void decimal5Converter_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5Converter().getAsObject(null, null, null));
	}

	@Test
	void decimal5Converter_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1.23456");
		String display = new Decimal5Converter().getAsString(null, null, original);
		assertNotNull(new Decimal5Converter().getAsObject(null, null, display));
	}

	@Test
	void decimal5Converter_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5Converter().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5Integer ----

	@Test
	void decimal5Integer_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5Integer().getAsString(null, null, null));
	}

	@Test
	void decimal5Integer_getAsStringFormatsValue() {
		assertNotNull(new Decimal5Integer().getAsString(null, null, new Decimal5("1234")));
	}

	@Test
	void decimal5Integer_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5Integer().getAsObject(null, null, null));
	}

	@Test
	void decimal5Integer_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1234");
		String display = new Decimal5Integer().getAsString(null, null, original);
		assertNotNull(new Decimal5Integer().getAsObject(null, null, display));
	}

	@Test
	void decimal5Integer_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5Integer().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5IntegerPercentage ----

	@Test
	void decimal5IntegerPercentage_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5IntegerPercentage().getAsString(null, null, null));
	}

	@Test
	void decimal5IntegerPercentage_getAsStringFormatsValue() {
		assertNotNull(new Decimal5IntegerPercentage().getAsString(null, null, new Decimal5("50")));
	}

	@Test
	void decimal5IntegerPercentage_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5IntegerPercentage().getAsObject(null, null, null));
	}

	@Test
	void decimal5IntegerPercentage_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("50");
		String display = new Decimal5IntegerPercentage().getAsString(null, null, original);
		assertNotNull(new Decimal5IntegerPercentage().getAsObject(null, null, display));
	}

	@Test
	void decimal5IntegerPercentage_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5IntegerPercentage().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5OneDecimalPlace ----

	@Test
	void decimal5OneDecimalPlace_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5OneDecimalPlace().getAsString(null, null, null));
	}

	@Test
	void decimal5OneDecimalPlace_getAsStringFormatsValue() {
		assertNotNull(new Decimal5OneDecimalPlace().getAsString(null, null, new Decimal5("1.5")));
	}

	@Test
	void decimal5OneDecimalPlace_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5OneDecimalPlace().getAsObject(null, null, null));
	}

	@Test
	void decimal5OneDecimalPlace_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1.5");
		String display = new Decimal5OneDecimalPlace().getAsString(null, null, original);
		assertNotNull(new Decimal5OneDecimalPlace().getAsObject(null, null, display));
	}

	@Test
	void decimal5OneDecimalPlace_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5OneDecimalPlace().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5TimeDuration ----

	@Test
	void decimal5TimeDuration_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5TimeDuration().getAsString(null, null, null));
	}

	@Test
	void decimal5TimeDuration_getAsStringFormatsValue() {
		assertNotNull(new Decimal5TimeDuration().getAsString(null, null, new Decimal5("1.5")));
	}

	@Test
	void decimal5TimeDuration_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5TimeDuration().getAsObject(null, null, null));
	}

	@Test
	void decimal5TimeDuration_getAsObjectParsesValidDuration() {
		assertNotNull(new Decimal5TimeDuration().getAsObject(null, null, "1:30"));
	}

	@Test
	void decimal5TimeDuration_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5TimeDuration().getAsObject(null, null, "not-a-duration"));
	}

	// ---- Decimal5TwoDecimalPlaces ----

	@Test
	void decimal5TwoDecimalPlaces_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5TwoDecimalPlaces().getAsString(null, null, null));
	}

	@Test
	void decimal5TwoDecimalPlaces_getAsStringFormatsValue() {
		assertNotNull(new Decimal5TwoDecimalPlaces().getAsString(null, null, new Decimal5("1.23")));
	}

	@Test
	void decimal5TwoDecimalPlaces_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5TwoDecimalPlaces().getAsObject(null, null, null));
	}

	@Test
	void decimal5TwoDecimalPlaces_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1.23");
		String display = new Decimal5TwoDecimalPlaces().getAsString(null, null, original);
		assertNotNull(new Decimal5TwoDecimalPlaces().getAsObject(null, null, display));
	}

	@Test
	void decimal5TwoDecimalPlaces_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5TwoDecimalPlaces().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5TwoDecimalPlacesPercentage ----

	@Test
	void decimal5TwoDecimalPlacesPercentage_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5TwoDecimalPlacesPercentage().getAsString(null, null, null));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentage_getAsStringFormatsValue() {
		assertNotNull(new Decimal5TwoDecimalPlacesPercentage().getAsString(null, null, new Decimal5("1.23")));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentage_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5TwoDecimalPlacesPercentage().getAsObject(null, null, null));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentage_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1.23");
		String display = new Decimal5TwoDecimalPlacesPercentage().getAsString(null, null, original);
		assertNotNull(new Decimal5TwoDecimalPlacesPercentage().getAsObject(null, null, display));
	}

	@Test
	void decimal5TwoDecimalPlacesPercentage_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5TwoDecimalPlacesPercentage().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal10Converter ----

	@Test
	void decimal10Converter_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal10Converter().getAsString(null, null, null));
	}

	@Test
	void decimal10Converter_getAsStringFormatsValue() {
		assertNotNull(new Decimal10Converter().getAsString(null, null, new Decimal10("1.23")));
	}

	@Test
	void decimal10Converter_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal10Converter().getAsObject(null, null, null));
	}

	@Test
	void decimal10Converter_getAsObjectParsesRoundTrip() {
		Decimal10 original = new Decimal10("1.23");
		String display = new Decimal10Converter().getAsString(null, null, original);
		assertNotNull(new Decimal10Converter().getAsObject(null, null, display));
	}

	@Test
	void decimal10Converter_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal10Converter().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal10TwoDecimalPlaces ----

	@Test
	void decimal10TwoDecimalPlaces_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal10TwoDecimalPlaces().getAsString(null, null, null));
	}

	@Test
	void decimal10TwoDecimalPlaces_getAsStringFormatsValue() {
		assertNotNull(new Decimal10TwoDecimalPlaces().getAsString(null, null, new Decimal10("1.23")));
	}

	@Test
	void decimal10TwoDecimalPlaces_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal10TwoDecimalPlaces().getAsObject(null, null, null));
	}

	@Test
	void decimal10TwoDecimalPlaces_getAsObjectParsesRoundTrip() {
		Decimal10 original = new Decimal10("1.23");
		String display = new Decimal10TwoDecimalPlaces().getAsString(null, null, original);
		assertNotNull(new Decimal10TwoDecimalPlaces().getAsObject(null, null, display));
	}

	@Test
	void decimal10TwoDecimalPlaces_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal10TwoDecimalPlaces().getAsObject(null, null, "not-a-number"));
	}
}
