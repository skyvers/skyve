package org.skyve.impl.bizport;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.impl.bizport.DataFileField.LoadAction;

@SuppressWarnings("static-method")
public class DataFileFieldTest {

	// ---- LoadAction enum ----

	@Test
	public void loadActionValuesContainsFiveValues() {
		LoadAction[] values = LoadAction.values();
		assertNotNull(values);
		assertEquals(5, values.length);
	}

	@Test
	public void loadActionValueOfSetValue() {
		assertEquals(LoadAction.SET_VALUE, LoadAction.valueOf("SET_VALUE"));
	}

	@Test
	public void loadActionValueOfLookupEquals() {
		assertEquals(LoadAction.LOOKUP_EQUALS, LoadAction.valueOf("LOOKUP_EQUALS"));
	}

	@Test
	public void loadActionValueOfLookupLike() {
		assertEquals(LoadAction.LOOKUP_LIKE, LoadAction.valueOf("LOOKUP_LIKE"));
	}

	@Test
	public void loadActionValueOfLookupContains() {
		assertEquals(LoadAction.LOOKUP_CONTAINS, LoadAction.valueOf("LOOKUP_CONTAINS"));
	}

	@Test
	public void loadActionValueOfConfirmValue() {
		assertEquals(LoadAction.CONFIRM_VALUE, LoadAction.valueOf("CONFIRM_VALUE"));
	}

	// ---- DataFileField constructors ----

	@Test
	public void bindingConstructorSetsBindingAndDefaults() {
		DataFileField f = new DataFileField("name");
		assertEquals("name", f.getBinding());
		assertEquals(LoadAction.SET_VALUE, f.getLoadAction());
		assertFalse(f.isRequired());
		assertNull(f.getIndex());
	}

	@Test
	public void bindingIndexConstructorSetsIndex() {
		DataFileField f = new DataFileField("code", 3);
		assertEquals("code", f.getBinding());
		assertEquals(Integer.valueOf(3), f.getIndex());
		assertEquals(LoadAction.SET_VALUE, f.getLoadAction());
		assertFalse(f.isRequired());
	}

	@Test
	public void fourArgConstructorSetsFields() {
		DataFileField f = new DataFileField("field", LoadAction.LOOKUP_EQUALS, true, 2);
		assertEquals("field", f.getBinding());
		assertEquals(LoadAction.LOOKUP_EQUALS, f.getLoadAction());
		assertEquals(Integer.valueOf(2), f.getIndex());
	}

	@Test
	public void fiveArgConstructorSetsConverterField() {
		DataFileField f = new DataFileField("field", LoadAction.LOOKUP_LIKE, false, 1, null);
		assertEquals("field", f.getBinding());
		assertEquals(LoadAction.LOOKUP_LIKE, f.getLoadAction());
		assertNull(f.getConverter());
	}

	// ---- Getters/Setters ----

	@Test
	public void setBindingRoundtrip() {
		DataFileField f = new DataFileField("original");
		f.setBinding("updated");
		assertEquals("updated", f.getBinding());
	}

	@Test
	public void setLoadActionRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setLoadAction(LoadAction.CONFIRM_VALUE);
		assertEquals(LoadAction.CONFIRM_VALUE, f.getLoadAction());
	}

	@Test
	public void setRequiredRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setRequired(true);
		assertTrue(f.isRequired());
	}

	@Test
	public void setIndexIntRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setIndex(7);
		assertEquals(Integer.valueOf(7), f.getIndex());
	}

	@Test
	public void setIndexIntegerRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setIndex(Integer.valueOf(5));
		assertEquals(Integer.valueOf(5), f.getIndex());
	}

	@Test
	public void setTreatEmptyNumericAsZeroRoundtrip() {
		DataFileField f = new DataFileField("x");
		f.setTreatEmptyNumericAsZero(true);
		assertTrue(f.isTreatEmptyNumericAsZero());
	}

	@Test
	public void setAttributeRoundtrip() {
		DataFileField f = new DataFileField("x");
		assertNull(f.getAttribute());
		// Setting null is valid
		f.setAttribute(null);
		assertNull(f.getAttribute());
	}

	@Test
	public void setConverterRoundtrip() {
		DataFileField f = new DataFileField("x");
		assertNull(f.getConverter());
		f.setConverter(null);
		assertNull(f.getConverter());
	}
}
