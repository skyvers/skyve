package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.ConverterName;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.metadata.model.document.DomainType;

/**
 * Tests for {@link FluentField}, {@link FluentConstrainableField}, and
 * {@link FluentConvertibleField} base class methods, exercised via the
 * concrete {@link FluentText} subclass.
 */
@SuppressWarnings("static-method")
class FluentFieldBaseTest {

	// --- FluentField methods ---

	@Test
	void requiredTrueSetsFlag() {
		FluentText f = new FluentText().required(true);
		assertTrue(f.get().isRequired());
	}

	@Test
	void requiredFalseClearsFlag() {
		FluentText f = new FluentText().required(false);
		assertFalse(f.get().isRequired());
	}

	@Test
	void requiredMessageSetsValue() {
		FluentText f = new FluentText().requiredMessage("This field is required");
		assertEquals("This field is required", f.get().getRequiredMessage());
	}

	@Test
	void persistentTrueSetsFlag() {
		FluentText f = new FluentText().persistent(true);
		assertTrue(f.get().isPersistent());
	}

	@Test
	void persistentFalseClearsFlag() {
		FluentText f = new FluentText().persistent(false);
		assertFalse(f.get().isPersistent());
	}

	@Test
	void dynamicTrueSetsFlag() {
		FluentText f = new FluentText().dynamic(true);
		assertTrue(f.get().isDynamic());
	}

	@Test
	void dynamicFalseClearsFlag() {
		FluentText f = new FluentText().dynamic(false);
		assertFalse(f.get().isDynamic());
	}

	@Test
	void indexDatabaseSetsValue() {
		FluentText f = new FluentText().index(IndexType.database);
		assertEquals(IndexType.database, f.get().getIndex());
	}

	@Test
	void indexTextualSetsValue() {
		FluentText f = new FluentText().index(IndexType.textual);
		assertEquals(IndexType.textual, f.get().getIndex());
	}

	@Test
	void defaultValueSetsValue() {
		FluentText f = new FluentText().defaultValue("defaultText");
		assertEquals("defaultText", f.get().getDefaultValue());
	}

	@Test
	void generatedInsertSetsValue() {
		FluentText f = new FluentText().generated(GeneratedType.insert);
		assertEquals(GeneratedType.insert, f.get().getGenerated());
	}

	@Test
	void generatedAlwaysSetsValue() {
		FluentText f = new FluentText().generated(GeneratedType.always);
		assertEquals(GeneratedType.always, f.get().getGenerated());
	}

	@Test
	void fromCopiesFieldProperties() {
		FluentText source = new FluentText()
				.required(true)
				.requiredMessage("required!")
				.persistent(false)
				.dynamic(true)
				.index(IndexType.both)
				.defaultValue("dv")
				.generated(GeneratedType.always);

		FluentText copy = new FluentText();
		copy.from(source.get());

		assertTrue(copy.get().isRequired());
		assertEquals("required!", copy.get().getRequiredMessage());
		assertFalse(copy.get().isPersistent());
		assertTrue(copy.get().isDynamic());
		assertEquals(IndexType.both, copy.get().getIndex());
		assertEquals("dv", copy.get().getDefaultValue());
		assertEquals(GeneratedType.always, copy.get().getGenerated());
	}

	// --- FluentConstrainableField methods ---

	@Test
	void domainTypeConstantSetsValue() {
		FluentText f = new FluentText().domainType(DomainType.constant);
		assertEquals(DomainType.constant, f.get().getDomainType());
	}

	@Test
	void domainTypeVariantSetsValue() {
		FluentText f = new FluentText().domainType(DomainType.variant);
		assertEquals(DomainType.variant, f.get().getDomainType());
	}

	@Test
	void fromCopiesDomainType() {
		FluentText source = new FluentText().domainType(DomainType.constant);
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertEquals(DomainType.constant, copy.get().getDomainType());
	}

	// --- FluentConvertibleField methods ---

	@Test
	void converterNameSetsValue() {
		FluentText f = new FluentText().converterName(ConverterName.DD_MM_YYYY);
		assertEquals(ConverterName.DD_MM_YYYY, f.get().getConverterName());
	}

	@Test
	void fromCopiesConverterName() {
		FluentText source = new FluentText().converterName(ConverterName.YYYY_MM_DD);
		FluentText copy = new FluentText();
		copy.from(source.get());
		assertEquals(ConverterName.YYYY_MM_DD, copy.get().getConverterName());
	}
}
