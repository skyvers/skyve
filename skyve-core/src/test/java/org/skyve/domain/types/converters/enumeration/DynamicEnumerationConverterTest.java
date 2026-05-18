package org.skyve.domain.types.converters.enumeration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.metadata.model.Attribute.AttributeType;

class DynamicEnumerationConverterTest {

	private DynamicEnumerationConverter converter;

	private static Enumeration buildEnumeration(boolean withDescription, boolean withName) {
		Enumeration e = new Enumeration();
		EnumeratedValue v1 = new EnumeratedValue();
		v1.setCode("A");
		if (withDescription) {
			v1.setDescription("Alpha");
		}
		if (withName) {
			v1.setName("a");
		}
		e.getXmlValues().add(v1);

		EnumeratedValue v2 = new EnumeratedValue();
		v2.setCode("B");
		if (withDescription) {
			v2.setDescription("Beta");
		}
		if (withName) {
			v2.setName("b");
		}
		e.getXmlValues().add(v2);
		return e;
	}

	@BeforeEach
	void setUp() {
		converter = new DynamicEnumerationConverter(buildEnumeration(true, true));
	}

	@Test
	@SuppressWarnings("static-method")
	void getValueTypeReturnsString() {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(true, true));
		assertEquals(String.class, c.getValueType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getAttributeTypeReturnsEnumeration() {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(true, true));
		assertEquals(AttributeType.enumeration, c.getAttributeType());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatReturnsNull() {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(true, true));
		assertNull(c.getFormat());
	}

	@Test
	@SuppressWarnings("static-method")
	void getValidatorReturnsNull() {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(true, true));
		assertNull(c.getValidator());
	}

	@Test
	@SuppressWarnings("static-method")
	void getFormatPatternReturnsNull() {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(true, true));
		assertNull(c.getFormatPattern());
	}

	@Test
	void toDisplayValueByCode() throws ConversionException {
		assertEquals("Alpha", converter.toDisplayValue("A"));
	}

	@Test
	void toDisplayValueByName() throws ConversionException {
		// "a" is not a code so falls through to name check
		assertEquals("Alpha", converter.toDisplayValue("a"));
	}

	@Test
	void toDisplayValueByDescription() throws ConversionException {
		// "Alpha" is the description — it is also not a code or name
		assertEquals("Alpha", converter.toDisplayValue("Alpha"));
	}

	@Test
	void toDisplayValueUnknownReturnsEmpty() throws ConversionException {
		assertEquals("", converter.toDisplayValue("Z"));
	}

	@Test
	@SuppressWarnings("static-method")
	void toDisplayValueNullDescriptionFallsBackToCode() throws ConversionException {
		DynamicEnumerationConverter c = new DynamicEnumerationConverter(buildEnumeration(false, false));
		// when description is null, display falls back to code
		assertEquals("A", c.toDisplayValue("A"));
	}

	@Test
	void fromDisplayValueByDescription() throws ConversionException {
		assertEquals("A", converter.fromDisplayValue("Alpha"));
	}

	@Test
	void fromDisplayValueByCode() throws ConversionException {
		// If description doesn't match but code does, return code
		assertEquals("A", converter.fromDisplayValue("A"));
	}

	@Test
	void fromDisplayValueByName() throws ConversionException {
		// "a" is a name — falls through to name check
		assertEquals("A", converter.fromDisplayValue("a"));
	}

	@Test
	void fromDisplayValueUnknownThrows() {
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("Z"));
	}
}
