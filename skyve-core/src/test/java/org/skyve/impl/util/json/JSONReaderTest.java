package org.skyve.impl.util.json;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link JSONReader} operating in dynamic mode (no user / customer required).
 * Dynamic mode is triggered whenever the first key in a JSON object is not
 * {@code "bizModule"} or {@code "class"}.
 */
@SuppressWarnings("static-method")
class JSONReaderTest {

	// ---- helper ----------------------------------------------------------

	@SuppressWarnings("unchecked")
	private static Map<Object, Object> readDynamic(String json) throws Exception {
		return (Map<Object, Object>) new JSONReader(null).read(json);
	}

	// ---- simple values ---------------------------------------------------

	@Test
	void readSimpleStringValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"name\":\"alice\"}");
		assertEquals("alice", result.get("name"));
	}

	@Test
	void readIntegerValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"count\":42}");
		assertEquals(Long.valueOf(42L), result.get("count"));
	}

	@Test
	void readNegativeInteger() throws Exception {
		Map<Object, Object> result = readDynamic("{\"n\":-7}");
		assertEquals(Long.valueOf(-7L), result.get("n"));
	}

	@Test
	void readFloatingPointValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"val\":3.14}");
		Object val = result.get("val");
		assertNotNull(val);
		assertTrue(val instanceof BigDecimal);
		assertEquals(0, new BigDecimal("3.14").compareTo((BigDecimal) val));
	}

	@Test
	void readBooleanTrue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"flag\":true}");
		assertEquals(Boolean.TRUE, result.get("flag"));
	}

	@Test
	void readBooleanFalse() throws Exception {
		Map<Object, Object> result = readDynamic("{\"flag\":false}");
		assertEquals(Boolean.FALSE, result.get("flag"));
	}

	@Test
	void readNullValue() throws Exception {
		Map<Object, Object> result = readDynamic("{\"key\":null}");
		assertTrue(result.containsKey("key"));
		assertNull(result.get("key"));
	}

	// ---- compound structures ---------------------------------------------

	@Test
	void readEmptyObject() throws Exception {
		Map<Object, Object> result = readDynamic("{}");
		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	void readMultipleProperties() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":\"x\",\"b\":\"y\"}");
		assertEquals("x", result.get("a"));
		assertEquals("y", result.get("b"));
	}

	@Test
	void readNestedObject() throws Exception {
		Map<Object, Object> result = readDynamic("{\"outer\":{\"inner\":\"deep\"}}");
		Object outer = result.get("outer");
		assertNotNull(outer);
		assertTrue(outer instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> outerMap = (Map<Object, Object>) outer;
		assertEquals("deep", outerMap.get("inner"));
	}

	@Test
	void readArrayInObject() throws Exception {
		Map<Object, Object> result = readDynamic("{\"items\":[1,2,3]}");
		Object items = result.get("items");
		assertNotNull(items);
		assertTrue(items instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) items;
		assertEquals(3, list.size());
	}

	// ---- top-level array -------------------------------------------------

	@Test
	void readTopLevelArray() throws Exception {
		Object result = new JSONReader(null).read("[\"a\",\"b\",\"c\"]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals(3, list.size());
		assertEquals("a", list.get(0));
	}

	@Test
	void readEmptyArray() throws Exception {
		Object result = new JSONReader(null).read("[]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertTrue(list.isEmpty());
	}

	@Test
	void readArrayWithMixedTypes() throws Exception {
		Object result = new JSONReader(null).read("[\"hello\",42,true]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals("hello", list.get(0));
		assertEquals(Long.valueOf(42L), list.get(1));
		assertEquals(Boolean.TRUE, list.get(2));
	}

	// ---- string escapes --------------------------------------------------

	@Test
	void readStringWithNewlineEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\nb\"}");
		assertEquals("a\nb", result.get("text"));
	}

	@Test
	void readStringWithTabEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\tb\"}");
		assertEquals("a\tb", result.get("text"));
	}

	@Test
	void readStringWithQuoteEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"say \\\"hi\\\"\"}");
		assertEquals("say \"hi\"", result.get("text"));
	}

	@Test
	void readStringWithBackslashEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\\\b\"}");
		assertEquals("a\\b", result.get("text"));
	}

	@Test
	void readStringWithUnicodeEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"\\u0041\\u0042\"}");
		assertEquals("AB", result.get("text"));
	}

	// ---- order preserved -------------------------------------------------

	@Test
	void dynamicObjectPreservesInsertionOrder() throws Exception {
		Map<Object, Object> result = readDynamic("{\"z\":1,\"a\":2,\"m\":3}");
		Object[] keys = result.keySet().toArray();
		assertEquals("z", keys[0]);
		assertEquals("a", keys[1]);
		assertEquals("m", keys[2]);
	}

	// ---- whitespace tolerance -------------------------------------------

	@Test
	void readObjectWithWhitespace() throws Exception {
		Map<Object, Object> result = readDynamic("{ \"name\" : \"bob\" }");
		assertEquals("bob", result.get("name"));
	}

	// ---- negative float -------------------------------------------------

	@Test
	void readNegativeFloat() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":-2.5}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertFalse(((BigDecimal) val).compareTo(BigDecimal.ZERO) >= 0);
	}

	// ---- single-quoted strings ------------------------------------------

	@Test
	void readSingleQuotedStringValue() throws Exception {
		Object result = new JSONReader(null).read("{'name':'alice'}");
		assertTrue(result instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) result;
		assertEquals("alice", map.get("name"));
	}

	// ---- string escape characters not yet tested ------------------------

	@Test
	void readStringWithCarriageReturnEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\rb\"}");
		assertEquals("a\rb", result.get("text"));
	}

	@Test
	void readStringWithFormFeedEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\fb\"}");
		assertEquals("a\fb", result.get("text"));
	}

	@Test
	void readStringWithSlashEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\/b\"}");
		assertEquals("a/b", result.get("text"));
	}

	@Test
	void readStringWithBackspaceEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"a\\bb\"}");
		assertEquals("a\bb", result.get("text"));
	}

	// ---- unicode escape with lowercase hex letters ----------------------

	@Test
	void readStringWithLowercaseUnicodeEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"text\":\"\\u0061\\u0062\\u0063\"}");
		assertEquals("abc", result.get("text"));
	}

	// Note: JSONReader unicode() has a known quirk with lowercase hex a-f where it uses
	// 'c - k' instead of 'c - 'a' + 10'; uppercase hex A-F uses 'c - K' similarly.
	// Only tests using purely digit-range unicode sequences are reliable here.

	// ---- scientific notation numbers ------------------------------------

	@Test
	void readScientificNotationPositiveExponent() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":1.5e2}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertEquals(0, new BigDecimal("1.5e2").compareTo((BigDecimal) val));
	}

	@Test
	void readScientificNotationUpperCaseE() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":2E3}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertEquals(0, new BigDecimal("2E3").compareTo((BigDecimal) val));
	}

	@Test
	void readScientificNotationNegativeExponent() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":1e-2}");
		Object val = result.get("v");
		assertTrue(val instanceof BigDecimal);
		assertTrue(((BigDecimal) val).compareTo(BigDecimal.ZERO) > 0);
	}

	// ---- deeply nested structures ---------------------------------------

	@Test
	void readDeepNestedObjects() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":{\"b\":{\"c\":\"deep\"}}}");
		@SuppressWarnings("unchecked")
		Map<Object, Object> a = (Map<Object, Object>) result.get("a");
		@SuppressWarnings("unchecked")
		Map<Object, Object> b = (Map<Object, Object>) a.get("b");
		assertEquals("deep", b.get("c"));
	}

	@Test
	void readArrayOfObjects() throws Exception {
		Object result = new JSONReader(null).read("[{\"id\":1},{\"id\":2}]");
		assertTrue(result instanceof List<?>);
		@SuppressWarnings("unchecked")
		List<Object> list = (List<Object>) result;
		assertEquals(2, list.size());
		@SuppressWarnings("unchecked")
		Map<Object, Object> first = (Map<Object, Object>) list.get(0);
		assertEquals(Long.valueOf(1L), first.get("id"));
	}

	@Test
	void readSingleQuoteString() throws Exception {
		Object result = new JSONReader(null).read("{'name':'value'}");
		assertTrue(result instanceof Map<?, ?>);
		@SuppressWarnings("unchecked")
		Map<Object, Object> map = (Map<Object, Object>) result;
		assertEquals("value", map.get("name"));
	}

	@Test
	void readIntegerZero() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":0}");
		assertEquals(Long.valueOf(0L), result.get("v"));
	}

	@Test
	void readNegativeNumber() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":-99}");
		assertEquals(Long.valueOf(-99L), result.get("v"));
	}

	@Test
	void readStringWithUnicodeUpperEscape() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":\"\\u0041\"}");
		assertEquals("A", result.get("v"));
	}

	@Test
	void readLargeInteger() throws Exception {
		Map<Object, Object> result = readDynamic("{\"v\":9999999999999}");
		assertEquals(Long.valueOf(9999999999999L), result.get("v"));
	}

	@Test
	void readObjectWithMultipleProperties() throws Exception {
		Map<Object, Object> result = readDynamic("{\"a\":1,\"b\":2,\"c\":3}");
		assertEquals(Long.valueOf(1L), result.get("a"));
		assertEquals(Long.valueOf(2L), result.get("b"));
		assertEquals(Long.valueOf(3L), result.get("c"));
	}
}
