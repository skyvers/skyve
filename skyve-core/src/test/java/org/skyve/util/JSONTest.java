package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link JSON} utility class - CORE-independent paths only.
 */
@SuppressWarnings("static-method")
class JSONTest {

	@Test
	void marshallNullReturnsNullJson() throws Exception {
		String result = JSON.marshall(null);
		assertEquals("null", result);
	}

	@Test
	void marshallStringProducesQuotedJson() throws Exception {
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
}
