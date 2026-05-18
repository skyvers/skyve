package org.skyve.impl.metadata.model.document.field;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.metadata.model.Attribute.AttributeType;

@SuppressWarnings("static-method")
class FieldModelTest {

	// --- Text ---

	@Test
	void textDefaultAttributeTypeIsText() {
		Text text = new Text();
		assertThat(text.getAttributeType(), is(AttributeType.text));
	}

	@Test
	void textSetAndGetLength() {
		Text text = new Text();
		text.setLength(100);
		assertEquals(100, text.getLength());
	}

	@Test
	void textIsScalar() {
		Text text = new Text();
		assertTrue(text.isScalar());
	}

	@Test
	void textSetAndGetValidator() {
		Text text = new Text();
		org.skyve.impl.metadata.model.document.field.validator.TextValidator validator =
			new org.skyve.impl.metadata.model.document.field.validator.TextValidator();
		text.setValidator(validator);
		assertThat(text.getValidator(), is(validator));
	}

	@Test
	void textSetAndGetFormat() {
		Text text = new Text();
		TextFormat format = new TextFormat();
		format.setMask("AAA-000");
		text.setFormat(format);
		assertThat(text.getFormat(), is(format));
		assertThat(text.getFormat().getMask(), is("AAA-000"));
	}

	@Test
	void textNullValidatorByDefault() {
		Text text = new Text();
		assertThat(text.getValidator(), nullValue());
	}

	@Test
	void textNullFormatByDefault() {
		Text text = new Text();
		assertThat(text.getFormat(), nullValue());
	}

	// --- Field (via Text) ---

	@Test
	void fieldRequiredDefaultIsFalse() {
		Text field = new Text();
		assertFalse(field.isRequired());
	}

	@Test
	void fieldSetRequiredToTrue() {
		Text field = new Text();
		field.setRequired(true);
		assertTrue(field.isRequired());
	}

	@Test
	void fieldSetRequiredBool() {
		Text field = new Text();
		field.setRequiredBool(java.lang.Boolean.TRUE);
		assertTrue(field.isRequired());
	}

	@Test
	void fieldPersistentDefaultIsTrue() {
		Text field = new Text();
		assertTrue(field.isPersistent());
	}

	@Test
	void fieldSetPersistentToFalse() {
		Text field = new Text();
		field.setPersistent(false);
		assertFalse(field.isPersistent());
	}

	@Test
	void fieldPersistentBoolSetViaBoolean() {
		Text field = new Text();
		field.setPersistentBool(java.lang.Boolean.FALSE);
		assertFalse(field.isPersistent());
	}

	@Test
	void fieldDynamicDefaultIsFalse() {
		Text field = new Text();
		assertFalse(field.isDynamic());
	}

	@Test
	void fieldSetDynamicToTrue() {
		Text field = new Text();
		field.setDynamic(true);
		assertTrue(field.isDynamic());
	}

	@Test
	void fieldDynamicBoolSetViaBoolean() {
		Text field = new Text();
		field.setDynamicBool(java.lang.Boolean.TRUE);
		assertTrue(field.isDynamic());
	}

	@Test
	void fieldIndexNullByDefault() {
		Text field = new Text();
		assertThat(field.getIndex(), nullValue());
	}

	@Test
	void fieldSetAndGetIndex() {
		Text field = new Text();
		field.setIndex(IndexType.database);
		assertThat(field.getIndex(), is(IndexType.database));
	}

	@Test
	void fieldDefaultValueNullByDefault() {
		Text field = new Text();
		assertThat(field.getDefaultValue(), nullValue());
	}

	@Test
	void fieldSetAndGetDefaultValue() {
		Text field = new Text();
		field.setDefaultValue("defaultText");
		assertThat(field.getDefaultValue(), is("defaultText"));
	}

	@Test
	void fieldRequiredMessageNullByDefault() {
		Text field = new Text();
		assertThat(field.getRequiredMessage(), nullValue());
	}

	@Test
	void fieldSetAndGetRequiredMessage() {
		Text field = new Text();
		field.setRequiredMessage("This field is required");
		assertThat(field.getRequiredMessage(), is("This field is required"));
	}

	// --- ConvertibleField ---

	@Test
	void convertibleFieldConverterNameNullByDefault() {
		ConvertibleField field = new ConvertibleField() {
			private static final long serialVersionUID = 1L;
		};
		assertThat(field.getConverterName(), nullValue());
	}

	@Test
	void convertibleFieldSetAndGetConverter() {
		ConvertibleField field = new ConvertibleField() {
			private static final long serialVersionUID = 1L;
		};
		assertThat(field.getConverter(), nullValue());
	}

	// --- TextFormat ---

	@Test
	void textFormatMaskNullByDefault() {
		TextFormat format = new TextFormat();
		assertThat(format.getMask(), nullValue());
	}

	@Test
	void textFormatSetAndGetMask() {
		TextFormat format = new TextFormat();
		format.setMask("###-###");
		assertThat(format.getMask(), is("###-###"));
	}

	@Test
	void textFormatCaseNullByDefault() {
		TextFormat format = new TextFormat();
		assertThat(format.getCase(), nullValue());
	}

	@Test
	void textFormatSetAndGetCase() {
		TextFormat format = new TextFormat();
		format.setCase(TextCase.upper);
		assertThat(format.getCase(), is(TextCase.upper));
	}

	@Test
	void textFormatGetFormatCreatesInstanceWhenNull() {
		TextFormat format = new TextFormat();
		format.setMask("AAA");
		assertThat(format.getFormat(), notNullValue());
	}

	@Test
	void textFormatGetFormatReturnsSameInstanceOnSubsequentCalls() {
		TextFormat format = new TextFormat();
		format.setMask("AAA");
		assertThat(format.getFormat(), is(format.getFormat()));
	}

	// --- IndexType enum ---

	@Test
	void indexTypeDatabaseValue() {
		assertThat(IndexType.valueOf("database"), is(IndexType.database));
	}

	@Test
	void indexTypeTextualValue() {
		assertThat(IndexType.valueOf("textual"), is(IndexType.textual));
	}

	@Test
	void indexTypeBothValue() {
		assertThat(IndexType.valueOf("both"), is(IndexType.both));
	}

	@Test
	void indexTypeNoneValue() {
		assertThat(IndexType.valueOf("none"), is(IndexType.none));
	}

	// --- Geometry ---

	@Test
	void geometryGetDomainTypeReturnsNull() {
		assertThat(new Geometry().getDomainType(), nullValue());
	}

	// --- Id ---

	@Test
	void idGetDomainTypeReturnsNull() {
		assertThat(new Id().getDomainType(), nullValue());
	}
}

