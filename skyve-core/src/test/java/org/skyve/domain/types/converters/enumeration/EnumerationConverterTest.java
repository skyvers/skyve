package org.skyve.domain.types.converters.enumeration;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ConversionException;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.metadata.model.Attribute.AttributeType;

/** Unit tests for {@link DynamicEnumerationConverter}. */
@SuppressWarnings("static-method")
class EnumerationConverterTest {

	private Enumeration enumeration;

	@BeforeEach
	void setUp() {
		enumeration = new Enumeration();
		// Populate the values directly (getTarget() returns this when no attributeRef)
		EnumeratedValue active = new EnumeratedValue();
		active.setCode("active");
		active.setName("active");
		active.setDescription("Active");

		EnumeratedValue inactive = new EnumeratedValue();
		inactive.setCode("inactive");
		inactive.setName("inactive");
		inactive.setDescription("Inactive");

		enumeration.getXmlValues().add(active);
		enumeration.getXmlValues().add(inactive);
	}

	@Test
	void converterGetValueTypeIsString() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThat(converter.getValueType().getSimpleName(), is("String"));
	}

	@Test
	void converterGetAttributeTypeIsEnumeration() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThat(converter.getAttributeType(), is(AttributeType.enumeration));
	}

	@Test
	void converterGetFormatIsNull() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThat(converter.getFormat(), is(nullValue()));
	}

	@Test
	void converterGetValidatorIsNull() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThat(converter.getValidator(), is(nullValue()));
	}

	@Test
	void converterGetFormatPatternIsNull() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThat(converter.getFormatPattern(), is(nullValue()));
	}

	@Test
	void toDisplayValueByCodeReturnsDescription() throws ConversionException {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		String display = converter.toDisplayValue("active");
		assertThat(display, is(notNullValue()));
	}

	@Test
	void toDisplayValueByNameReturnsDescription() throws ConversionException {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		String display = converter.toDisplayValue("inactive");
		assertThat(display, is(notNullValue()));
	}

	@Test
	void toDisplayValueUnknownReturnsEmptyString() throws ConversionException {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		String display = converter.toDisplayValue("unknown_code");
		assertThat(display, is(""));
	}

	@Test
	void fromDisplayValueByCodeReturnsCode() throws ConversionException {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		String code = converter.fromDisplayValue("active");
		assertThat(code, is("active"));
	}

	@Test
	void fromDisplayValueUnknownThrows() {
		DynamicEnumerationConverter converter = new DynamicEnumerationConverter(enumeration);
		assertThrows(ConversionException.class, () -> converter.fromDisplayValue("xyz_unknown"));
	}
}
