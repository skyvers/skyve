package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.metadata.customer.Customer;

/**
 * Tests for {@link JSON} utility class - CORE-independent paths only.
 */
@SuppressWarnings("static-method")
class JSONTest {
	/** Supplies a minimal record for public API round-trip verification. */
	private record ExampleRecord(String name, int count) {
		// Test value.
	}

	@Test
	void marshallNullReturnsNullJson() {
		String result = JSON.marshall(null);
		assertEquals("null", result);
	}

	@Test
	void marshallStringProducesQuotedJson() {
		String result = JSON.marshall("hello");
		assertEquals("\"hello\"", result);
	}

	@Test
	void unmarshallSimpleJsonStringReturnsValue() throws Exception {
		Object result = JSON.unmarshall("\"hello\"");
		assertEquals("hello", result);
	}

	@Test
	void unmarshallNullJsonReturnsNull() throws Exception {
		Object result = JSON.unmarshall("null");
		assertEquals(null, result);
	}

	@Test
	void unmarshallJsonNumberReturnsNumber() throws Exception {
		Object result = JSON.unmarshall("42");
		assertNotNull(result);
	}

	/** Verifies typed and self-describing record round trips through the public utility. */
	@Test
	void recordRoundTripSupportsTypedAndUntypedUnmarshalling() throws Exception {
		ExampleRecord expected = new ExampleRecord("example", 3);

		String json = JSON.marshall(expected);
		ExampleRecord typedResult = JSON.unmarshall(json, ExampleRecord.class);
		Object untypedResult = JSON.unmarshall(json);

		assertEquals("{\"class\":\"org.skyve.util.JSONTest$ExampleRecord\",\"name\":\"example\",\"count\":3}", json);
		assertEquals(expected, typedResult);
		assertEquals(expected, untypedResult);
	}

	@Test
	void unmarshallWithoutUserReconstructsJavaValuesAndNull() throws Exception {
		ExampleRecord expected = new ExampleRecord("example", 3);

		assertEquals(expected, JSON.unmarshall(null, JSON.marshall(expected)));
		assertNull(JSON.unmarshall(null, "null"));
	}

	@Test
	void marshallWithNullPropertyNamesWritesCompleteValue() {
		String json = JSON.marshall(null, new ExampleRecord("example", 3), null);

		assertEquals("{\"class\":\"org.skyve.util.JSONTest$ExampleRecord\",\"name\":\"example\",\"count\":3}", json);
	}

	// ---- marshall(Customer, Object) -- exercises the 2-arg Customer overload ----

	@Test
	void marshallWithNullCustomerAndObjectReturnsJson() {
		String result = JSON.marshall(Mockito.mock(Customer.class), "hello");
		assertNotNull(result);
	}

	// ---- marshall(Customer, Object, Set) -- exercises the 3-arg Customer overload ----

	@Test
	void marshallWithNullCustomerObjectAndPropertyNamesReturnsJson() {
		Set<String> props = new HashSet<>();
		String result = JSON.marshall(Mockito.mock(Customer.class), "hello", props);
		assertNotNull(result);
	}
}
