package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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
