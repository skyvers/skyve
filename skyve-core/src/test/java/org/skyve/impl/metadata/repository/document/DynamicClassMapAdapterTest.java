package org.skyve.impl.metadata.repository.document;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;

public class DynamicClassMapAdapterTest {

	// ---- unmarshal ----

	@Test
	@SuppressWarnings("static-method")
	void unmarshalEmptyTypeReturnsEmptyMap() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		DynamicClassMapType input = new DynamicClassMapType();
		Map<String, String> result = adapter.unmarshal(input);
		assertNotNull(result);
		assertEquals(0, result.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void unmarshalSingleEntryProducesCorrectMapping() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		DynamicClassMapType input = new DynamicClassMapType();
		DynamicClassMapEntryType entry = new DynamicClassMapEntryType();
		entry.name = "MyClass";
		entry.className = "com.example.MyClass";
		input.classes.add(entry);

		Map<String, String> result = adapter.unmarshal(input);
		assertEquals(1, result.size());
		assertThat(result.get("MyClass"), is("com.example.MyClass"));
	}

	@Test
	@SuppressWarnings("static-method")
	void unmarshalMultipleEntriesPreservesAll() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		DynamicClassMapType input = new DynamicClassMapType();

		DynamicClassMapEntryType e1 = new DynamicClassMapEntryType();
		e1.name = "Alpha";
		e1.className = "com.example.Alpha";
		input.classes.add(e1);

		DynamicClassMapEntryType e2 = new DynamicClassMapEntryType();
		e2.name = "Beta";
		e2.className = "com.example.Beta";
		input.classes.add(e2);

		Map<String, String> result = adapter.unmarshal(input);
		assertEquals(2, result.size());
		assertThat(result.get("Alpha"), is("com.example.Alpha"));
		assertThat(result.get("Beta"), is("com.example.Beta"));
	}

	// ---- marshal ----

	@Test
	@SuppressWarnings("static-method")
	void marshalEmptyMapReturnsNull() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		Map<String, String> input = new TreeMap<>();
		DynamicClassMapType result = adapter.marshal(input);
		assertNull(result);
	}

	@Test
	@SuppressWarnings("static-method")
	void marshalSingleEntryProducesCorrectType() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		Map<String, String> input = new TreeMap<>();
		input.put("Foo", "com.example.Foo");

		DynamicClassMapType result = adapter.marshal(input);
		assertNotNull(result);
		assertEquals(1, result.classes.size());
		assertThat(result.classes.get(0).name, is("Foo"));
		assertThat(result.classes.get(0).className, is("com.example.Foo"));
	}

	@Test
	@SuppressWarnings("static-method")
	void marshalRoundtripPreservesEntries() throws Exception {
		DynamicClassMapAdapter adapter = new DynamicClassMapAdapter();
		Map<String, String> original = new TreeMap<>();
		original.put("A", "com.example.A");
		original.put("B", "com.example.B");

		DynamicClassMapType marshalled = adapter.marshal(original);
		Map<String, String> unmarshalled = adapter.unmarshal(marshalled);
		assertEquals(2, unmarshalled.size());
		assertThat(unmarshalled.get("A"), is("com.example.A"));
		assertThat(unmarshalled.get("B"), is("com.example.B"));
	}
}
