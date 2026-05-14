package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class PropertyMapAdapterTest {

	@Test
	void unmarshalNullEntriesReturnsEmptyMap() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		PropertyMapType type = new PropertyMapType(); // empty list
		Map<String, String> result = adapter.unmarshal(type);
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void unmarshalSingleEntryReturnsEntry() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		PropertyMapType type = new PropertyMapType();
		PropertyMapEntryType entry = new PropertyMapEntryType();
		entry.key = "colour";
		entry.value = "blue";
		type.properties.add(entry);

		Map<String, String> result = adapter.unmarshal(type);
		assertEquals(1, result.size());
		assertEquals("blue", result.get("colour"));
	}

	@Test
	void unmarshalMultipleEntriesPreservesAll() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		PropertyMapType type = new PropertyMapType();
		for (String k : new String[]{"a", "b", "c"}) {
			PropertyMapEntryType e = new PropertyMapEntryType();
			e.key = k;
			e.value = k.toUpperCase();
			type.properties.add(e);
		}

		Map<String, String> result = adapter.unmarshal(type);
		assertEquals(3, result.size());
		assertEquals("A", result.get("a"));
		assertEquals("B", result.get("b"));
		assertEquals("C", result.get("c"));
	}

	@Test
	void marshalEmptyMapReturnsNull() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		Map<String, String> map = new TreeMap<>();
		PropertyMapType result = adapter.marshal(map);
		assertNull(result);
	}

	@Test
	void marshalSingleEntryProducesPropertyMapType() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		Map<String, String> map = new TreeMap<>();
		map.put("foo", "bar");

		PropertyMapType result = adapter.marshal(map);
		assertNotNull(result);
		assertEquals(1, result.properties.size());
		assertEquals("foo", result.properties.get(0).key);
		assertEquals("bar", result.properties.get(0).value);
	}

	@Test
	void marshalMultipleEntriesPreservesAll() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		Map<String, String> map = new TreeMap<>();
		map.put("x", "1");
		map.put("y", "2");

		PropertyMapType result = adapter.marshal(map);
		assertNotNull(result);
		assertEquals(2, result.properties.size());
	}

	@Test
	void roundtripPreservesAllEntries() throws Exception {
		PropertyMapAdapter adapter = new PropertyMapAdapter();
		Map<String, String> original = new TreeMap<>();
		original.put("alpha", "1");
		original.put("beta", "2");

		PropertyMapType marshalled = adapter.marshal(original);
		Map<String, String> roundtripped = adapter.unmarshal(marshalled);

		assertEquals(original, roundtripped);
	}
}
