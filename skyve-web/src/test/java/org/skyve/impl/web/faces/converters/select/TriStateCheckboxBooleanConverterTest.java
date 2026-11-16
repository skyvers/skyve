package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
class TriStateCheckboxBooleanConverterTest {

	private TriStateCheckboxBooleanConverter converter;

	@BeforeEach
	void before() {
		converter = new TriStateCheckboxBooleanConverter();
	}

	@Test
	void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	void getAsObjectReturnsNullForEmptyString() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	@Test
	void getAsObjectReturnsTrueForOne() {
		assertEquals(Boolean.TRUE, converter.getAsObject(null, null, "1"));
	}

	@Test
	void getAsObjectReturnsFalseForTwo() {
		assertEquals(Boolean.FALSE, converter.getAsObject(null, null, "2"));
	}

	@Test
	void getAsObjectReturnsNullForOtherValue() {
		assertNull(converter.getAsObject(null, null, "0"));
	}

	@Test
	void getAsStringReturnsZeroForNullValue() {
		assertEquals("0", converter.getAsString(null, null, null));
	}

	@Test
	void getAsStringReturnsOneForTrue() {
		assertEquals("1", converter.getAsString(null, null, Boolean.TRUE));
	}

	@Test
	void getAsStringReturnsTwoForFalse() {
		assertEquals("2", converter.getAsString(null, null, Boolean.FALSE));
	}
}
