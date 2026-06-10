package org.skyve.impl.web.faces.converters.lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
@SuppressWarnings("static-method")
public class LangConverterTest {

	// ---- Boolean ----

	@Test
	void boolean_getAsStringReturnsNullForNull() {
		assertNull(new Boolean().getAsString(null, null, null));
	}

	@Test
	void boolean_getAsStringFormatsTrue() {
		assertEquals("true", new Boolean().getAsString(null, null, java.lang.Boolean.TRUE));
	}

	@Test
	void boolean_getAsStringFormatsFalse() {
		assertEquals("false", new Boolean().getAsString(null, null, java.lang.Boolean.FALSE));
	}

	@Test
	void boolean_getAsObjectReturnsNullForNull() {
		assertNull(new Boolean().getAsObject(null, null, null));
	}

	@Test
	void boolean_getAsObjectReturnsNullForEmpty() {
		assertNull(new Boolean().getAsObject(null, null, "  "));
	}

	@Test
	void boolean_getAsObjectParsesTrue() {
		assertEquals(java.lang.Boolean.TRUE, new Boolean().getAsObject(null, null, "true"));
	}

	@Test
	void boolean_getAsObjectParsesFalse() {
		assertEquals(java.lang.Boolean.FALSE, new Boolean().getAsObject(null, null, "false"));
	}

	// ---- String ----

	@Test
	void string_getAsStringReturnsNullForNull() {
		assertNull(new String().getAsString(null, null, null));
	}

	@Test
	void string_getAsStringReturnsValue() {
		assertEquals("hello", new String().getAsString(null, null, "hello"));
	}

	@Test
	void string_getAsObjectReturnsNullForNull() {
		assertNull(new String().getAsObject(null, null, null));
	}

	@Test
	void string_getAsObjectReturnsNullForEmpty() {
		assertNull(new String().getAsObject(null, null, "  "));
	}

	@Test
	void string_getAsObjectReturnsTrimmedValue() {
		assertEquals("hello", new String().getAsObject(null, null, "  hello  "));
	}
}
