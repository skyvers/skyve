package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Before;
import org.junit.Test;

@SuppressWarnings("static-method")
public class TriStateCheckboxBooleanConverterTest {

	private TriStateCheckboxBooleanConverter converter;

	@Before
	public void before() {
		converter = new TriStateCheckboxBooleanConverter();
	}

	@Test
	public void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void getAsObjectReturnsNullForEmptyString() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	@Test
	public void getAsObjectReturnsTrueForOne() {
		assertEquals(Boolean.TRUE, converter.getAsObject(null, null, "1"));
	}

	@Test
	public void getAsObjectReturnsFalseForTwo() {
		assertEquals(Boolean.FALSE, converter.getAsObject(null, null, "2"));
	}

	@Test
	public void getAsObjectReturnsNullForOtherValue() {
		assertNull(converter.getAsObject(null, null, "0"));
	}

	@Test
	public void getAsStringReturnsZeroForNullValue() {
		assertEquals("0", converter.getAsString(null, null, null));
	}

	@Test
	public void getAsStringReturnsOneForTrue() {
		assertEquals("1", converter.getAsString(null, null, Boolean.TRUE));
	}

	@Test
	public void getAsStringReturnsTwoForFalse() {
		assertEquals("2", converter.getAsString(null, null, Boolean.FALSE));
	}
}
