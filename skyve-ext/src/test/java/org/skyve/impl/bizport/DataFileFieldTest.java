package org.skyve.impl.bizport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.bizport.DataFileField.LoadAction;

@SuppressWarnings("static-method")
class DataFileFieldTest {

	// ---- LoadAction enum ----

	@Test
	void loadActionValuesContainsFiveValues() {
		LoadAction[] values = LoadAction.values();
		assertNotNull(values);
		assertEquals(5, values.length);
	}

	@Test
	void loadActionValueOfSetValue() {
		assertEquals(LoadAction.SET_VALUE, LoadAction.valueOf("SET_VALUE"));
	}

	@Test
	void loadActionValueOfLookupEquals() {
		assertEquals(LoadAction.LOOKUP_EQUALS, LoadAction.valueOf("LOOKUP_EQUALS"));
	}

	@Test
	void loadActionValueOfLookupLike() {
		assertEquals(LoadAction.LOOKUP_LIKE, LoadAction.valueOf("LOOKUP_LIKE"));
	}

	@Test
	void loadActionValueOfLookupContains() {
		assertEquals(LoadAction.LOOKUP_CONTAINS, LoadAction.valueOf("LOOKUP_CONTAINS"));
	}

	@Test
	void loadActionValueOfConfirmValue() {
		assertEquals(LoadAction.CONFIRM_VALUE, LoadAction.valueOf("CONFIRM_VALUE"));
	}

	// ---- DataFileField constructors ----

	@Test
	void bindingConstructorSetsBindingAndDefaults() {
		DataFileField f = new DataFileField("name");
		assertEquals("name", f.getBinding());
		assertEquals(LoadAction.SET_VALUE, f.getLoadAction());
		assertFalse(f.isRequired());
		assertNull(f.getIndex());
	}

	@Test
	void bindingIndexConstructorSetsIndex() {
		DataFileField f = new DataFileField("code", 3);
		assertEquals("code", f.getBinding());
		assertEquals(Integer.valueOf(3), f.getIndex());
		assertEquals(LoadAction.SET_VALUE, f.getLoadAction());
		assertFalse(f.isRequired());
	}

	@Test
	void fourArgConstructorSetsFields() {
		DataFileField f = new DataFileField("field", LoadAction.LOOKUP_EQUALS, true, 2);
		assertEquals("field", f.getBinding());
		assertEquals(LoadAction.LOOKUP_EQUALS, f.getLoadAction());
		assertEquals(Integer.valueOf(2), f.getIndex());
	}

	@Test
	void fiveArgConstructorSetsConverterField() {
		DataFileField f = new DataFileField("field", LoadAction.LOOKUP_LIKE, false, 1, null);
		assertEquals("field", f.getBinding());
		assertEquals(LoadAction.LOOKUP_LIKE, f.getLoadAction());
		assertNull(f.getConverter());
	}

	// ---- Getters/Setters ----

	@Test
	void setBindingRoundtrip() {
		DataFileField f = new DataFileField("original");
		f.setBinding("updated");
		assertEquals("updated", f.getBinding());
	}

	@Test
	void setLoadActionRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setLoadAction(LoadAction.CONFIRM_VALUE);
		assertEquals(LoadAction.CONFIRM_VALUE, f.getLoadAction());
	}

	@Test
	void setRequiredRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setRequired(true);
		assertTrue(f.isRequired());
	}

	@Test
	void setIndexIntRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setIndex(7);
		assertEquals(Integer.valueOf(7), f.getIndex());
	}

	@Test
	void setIndexIntegerRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setIndex(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), f.getIndex());
	}

	@Test
	void setTreatEmptyNumericAsZeroRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setTreatEmptyNumericAsZero(true);
		assertTrue(f.isTreatEmptyNumericAsZero());
	}

	@Test
	void setAttributeRoundtrip() {
		DataFileField f = new DataFileField("x");
		assertNull(f.getAttribute());
		// Setting null is valid
		f.setAttribute(null);
		assertNull(f.getAttribute());
	}

	@Test
	void setConverterRoundtrip() {
		DataFileField f = new DataFileField("x");
		assertNull(f.getConverter());
		f.setConverter(null);
		assertNull(f.getConverter());
	}
}
