package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.faces.model.SelectItem;

public class SelectItemsBeanConverterTest {

	private SelectItemsBeanConverter converter;

	@Before
	public void before() {
		converter = new SelectItemsBeanConverter();
	}

	// ---- getAsObject ----

	@Test
	public void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void getAsObjectReturnsNullForEmptyString() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	// ---- getAsString ----

	@Test
	public void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	public void getAsStringDelegatesToToStringForOtherValues() {
		assertEquals("42", converter.getAsString(null, null, Integer.valueOf(42)));
	}

	@Test
	public void getAsStringUsesToCodeForEnumeration() {
		Enumeration e = new Enumeration() {
			@Override
			public String toCode() {
				return "TEST_CODE";
			}

			@Override
			public String toLocalisedDescription() {
				return "Test Description";
			}

			@Override
			public DomainValue toDomainValue() {
				return new DomainValue(toCode(), toLocalisedDescription());
			}
		};
		assertEquals("TEST_CODE", converter.getAsString(null, null, e));
	}

	// ---- isEmpty ----

	@Test
	@SuppressWarnings("static-method")
	public void isEmptyReturnsTrueForNull() {
		assertTrue(SelectItemsBeanConverter.isEmpty(null));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isEmptyReturnsTrueForEmptyArray() {
		assertTrue(SelectItemsBeanConverter.isEmpty(new SelectItem[0]));
	}

	@Test
	@SuppressWarnings("static-method")
	public void isEmptyReturnsFalseForNonEmptyArray() {
		assertFalse(SelectItemsBeanConverter.isEmpty(new SelectItem[]{new SelectItem("v", "l")}));
	}
}
