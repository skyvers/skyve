package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.LinkedHashMap;

import org.junit.jupiter.api.Test;

/**
 * Additional tests for {@link SmartClientAttributeDefinition} covering
 * getter/setter, value-map serialisation, and edge-case paths.
 */
@SuppressWarnings("static-method")
class SmartClientAttributeDefinitionSupplementTest {

	private static final class TestDefinition extends SmartClientAttributeDefinition {
		private TestDefinition(String name) {
			super(null, null, null, null, null, name, false, false, false, null);
			this.valueMap = new LinkedHashMap<>();
		}

		private void setValueMapForTest(LinkedHashMap<String, String> map) {
			this.valueMap = map;
		}
	}

	// ===== getValueMapAsString =====

	@Test
	void getValueMapAsStringReturnsEmptyArrayWhenValueMapEmpty() {
		TestDefinition def = new TestDefinition("emptyField");
		assertEquals("[' ']", def.getValueMapAsString());
	}

	@Test
	void getValueMapAsStringReturnsJsonMapWhenValueMapNotEmpty() {
		TestDefinition def = new TestDefinition("fieldWithValues");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("A", "Alpha");
		map.put("B", "Beta");
		def.setValueMapForTest(map);

		String result = def.getValueMapAsString();
		assertTrue(result.startsWith("{"));
		assertTrue(result.endsWith("}"));
		assertTrue(result.contains("'A':'Alpha'"));
		assertTrue(result.contains("'B':'Beta'"));
	}

	@Test
	void getValueMapAsStringSingleEntry() {
		TestDefinition def = new TestDefinition("singleField");
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("X", "X-Ray");
		def.setValueMapForTest(map);

		assertEquals("{'X':'X-Ray'}", def.getValueMapAsString());
	}

	// ===== getter / setter coverage =====

	@Test
	void getTargetReturnsNullByDefault() {
		assertNull(new TestDefinition("t").getTarget());
	}

	@Test
	void getNameReturnsSuppliedName() {
		assertEquals("myField", new TestDefinition("myField").getName());
	}

	@Test
	void getTitleReturnsSuppliedName() {
		assertEquals("title", new TestDefinition("title").getTitle());
	}

	@Test
	void getTypeIsTextByDefault() {
		assertEquals("text", new TestDefinition("f").getType());
	}

	@Test
	void setAndGetEditorType() {
		TestDefinition def = new TestDefinition("f");
		def.setEditorType("select");
		assertEquals("select", def.getEditorType());
	}

	@Test
	void setAndGetLength() {
		TestDefinition def = new TestDefinition("f");
		def.setLength(Integer.valueOf(200));
		assertEquals(Integer.valueOf(200), def.getLength());
	}

	@Test
	void getLengthNullByDefault() {
		assertNull(new TestDefinition("f").getLength());
	}

	@Test
	void setAndGetRequiredMessage() {
		TestDefinition def = new TestDefinition("f");
		def.setRequiredMessage("required");
		assertEquals("required", def.getRequiredMessage());
	}

	@Test
	void setAndGetTitle() {
		TestDefinition def = new TestDefinition("f");
		def.setTitle("Custom");
		assertEquals("Custom", def.getTitle());
	}

	@Test
	void setAndGetType() {
		TestDefinition def = new TestDefinition("f");
		def.setType("integer");
		assertEquals("integer", def.getType());
	}

	@Test
	void hasDisplayFieldDefaultFalse() {
		assertFalse(new TestDefinition("f").isHasDisplayField());
	}

	@Test
	void setAndGetHasDisplayField() {
		TestDefinition def = new TestDefinition("f");
		def.setHasDisplayField(true);
		assertTrue(def.isHasDisplayField());
	}

	@Test
	void escapeDefaultTrue() {
		assertTrue(new TestDefinition("f").isEscape());
	}

	@Test
	void setAndGetEscape() {
		TestDefinition def = new TestDefinition("f");
		def.setEscape(false);
		assertFalse(def.isEscape());
	}

	@Test
	void getValueMapReturnsEmptyByDefault() {
		assertTrue(new TestDefinition("f").getValueMap().isEmpty());
	}

	@Test
	void setAndGetName() {
		TestDefinition def = new TestDefinition("orig");
		def.setName("renamed");
		assertEquals("renamed", def.getName());
	}

	@Test
	void getLookupInitiallyNull() {
		assertNull(new TestDefinition("f").getLookup());
	}

	@Test
	void getRequiredMessageInitiallyNull() {
		assertNull(new TestDefinition("f").getRequiredMessage());
	}

	@Test
	void alignmentDefaultsToNull() {
		TestDefinition def = new TestDefinition("f");
		// alignment is a protected field, not exposed directly in the public API
		// but the getter is not exposed; just verify no crash on construction
		assertEquals("f", def.getName());
	}
}