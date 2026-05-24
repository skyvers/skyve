package org.skyve.impl.web.faces.converters.decimal.currency;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;

import jakarta.faces.convert.ConverterException;

public class CurrencyConverterTest {

	// ---- Decimal2DollarsAndCents ----

	@Test
	void decimal2DollarsAndCents_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2DollarsAndCents().getAsString(null, null, null));
	}

	@Test
	void decimal2DollarsAndCents_getAsStringFormatsValue() {
		assertNotNull(new Decimal2DollarsAndCents().getAsString(null, null, new Decimal2("1234.50")));
	}

	@Test
	void decimal2DollarsAndCents_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2DollarsAndCents().getAsObject(null, null, null));
	}

	@Test
	void decimal2DollarsAndCents_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("1234.50");
		String display = new Decimal2DollarsAndCents().getAsString(null, null, original);
		assertNotNull(new Decimal2DollarsAndCents().getAsObject(null, null, display));
	}

	@Test
	void decimal2DollarsAndCents_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2DollarsAndCents().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal2DollarsAndCentsAbsolute ----

	@Test
	void decimal2DollarsAndCentsAbsolute_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal2DollarsAndCentsAbsolute().getAsString(null, null, null));
	}

	@Test
	void decimal2DollarsAndCentsAbsolute_getAsStringFormatsValue() {
		assertNotNull(new Decimal2DollarsAndCentsAbsolute().getAsString(null, null, new Decimal2("1234.50")));
	}

	@Test
	void decimal2DollarsAndCentsAbsolute_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal2DollarsAndCentsAbsolute().getAsObject(null, null, null));
	}

	@Test
	void decimal2DollarsAndCentsAbsolute_getAsObjectParsesRoundTrip() {
		Decimal2 original = new Decimal2("1234.50");
		String display = new Decimal2DollarsAndCentsAbsolute().getAsString(null, null, original);
		assertNotNull(new Decimal2DollarsAndCentsAbsolute().getAsObject(null, null, display));
	}

	@Test
	void decimal2DollarsAndCentsAbsolute_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal2DollarsAndCentsAbsolute().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal5DollarsAndCents ----

	@Test
	void decimal5DollarsAndCents_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal5DollarsAndCents().getAsString(null, null, null));
	}

	@Test
	void decimal5DollarsAndCents_getAsStringFormatsValue() {
		assertNotNull(new Decimal5DollarsAndCents().getAsString(null, null, new Decimal5("1234.50")));
	}

	@Test
	void decimal5DollarsAndCents_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal5DollarsAndCents().getAsObject(null, null, null));
	}

	@Test
	void decimal5DollarsAndCents_getAsObjectParsesRoundTrip() {
		Decimal5 original = new Decimal5("1234.50");
		String display = new Decimal5DollarsAndCents().getAsString(null, null, original);
		assertNotNull(new Decimal5DollarsAndCents().getAsObject(null, null, display));
	}

	@Test
	void decimal5DollarsAndCents_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal5DollarsAndCents().getAsObject(null, null, "not-a-number"));
	}

	// ---- Decimal10DollarsAndCents ----

	@Test
	void decimal10DollarsAndCents_getAsStringReturnsEmptyForNull() {
		assertEquals("", new Decimal10DollarsAndCents().getAsString(null, null, null));
	}

	@Test
	void decimal10DollarsAndCents_getAsStringFormatsValue() {
		assertNotNull(new Decimal10DollarsAndCents().getAsString(null, null, new Decimal10("1234.50")));
	}

	@Test
	void decimal10DollarsAndCents_getAsObjectReturnsNullForNull() {
		assertNull(new Decimal10DollarsAndCents().getAsObject(null, null, null));
	}

	@Test
	void decimal10DollarsAndCents_getAsObjectParsesRoundTrip() {
		Decimal10 original = new Decimal10("1234.50");
		String display = new Decimal10DollarsAndCents().getAsString(null, null, original);
		assertNotNull(new Decimal10DollarsAndCents().getAsObject(null, null, display));
	}

	@Test
	void decimal10DollarsAndCents_getAsObjectThrowsForInvalid() {
		assertThrows(ConverterException.class, () -> new Decimal10DollarsAndCents().getAsObject(null, null, "not-a-number"));
	}
}
