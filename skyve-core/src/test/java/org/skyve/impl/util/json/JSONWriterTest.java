package org.skyve.impl.util.json;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class JSONWriterTest {

	@Test
	void staticWriteLong() {
		assertThat(JSONWriter.write(42L), is("42"));
	}

	@Test
	void staticWriteNegativeLong() {
		assertThat(JSONWriter.write(-1L), is("-1"));
	}

	@Test
	void staticWriteDouble() {
		assertThat(JSONWriter.write(3.14d), is("3.14"));
	}

	@Test
	void staticWriteChar() {
		assertThat(JSONWriter.write('a'), is("\"a\""));
	}

	@Test
	void staticWriteBooleanTrue() {
		assertThat(JSONWriter.write(true), is("true"));
	}

	@Test
	void staticWriteBooleanFalse() {
		assertThat(JSONWriter.write(false), is("false"));
	}

	@Test
	void writeNullReturnsNullLiteral() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(null, null);
		assertThat(result, is("null"));
	}

	@Test
	void writeStringProducesQuotedValue() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("hello", null);
		assertThat(result, is("\"hello\""));
	}

	@Test
	void writeStringWithSpecialChars() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("say \"hi\"", null);
		assertThat(result, is("\"say \\\"hi\\\"\""));
	}

	@Test
	void writeStringWithBackslash() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\\b", null);
		assertThat(result, is("\"a\\\\b\""));
	}

	@Test
	void writeStringWithNewline() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("line1\nline2", null);
		assertThat(result, is("\"line1\\nline2\""));
	}

	@Test
	void writeStringWithTab() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\tb", null);
		assertThat(result, is("\"a\\tb\""));
	}

	@Test
	void writeStringWithCarriageReturn() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\rb", null);
		assertThat(result, is("\"a\\rb\""));
	}

	@Test
	void writeIntegerNumber() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Integer.valueOf(7), null);
		assertThat(result, is("7"));
	}

	@Test
	void writeLongNumber() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Long.valueOf(100L), null);
		assertThat(result, is("100"));
	}

	@Test
	void writeBoolean() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Boolean.TRUE, null);
		assertThat(result, is("true"));
	}

	@Test
	void writeEmptyMap() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new LinkedHashMap<>(), null);
		assertThat(result, is("{}"));
	}

	@Test
	void writeSingleEntryMap() {
		JSONWriter writer = new JSONWriter(null);
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("key", "value");
		String result = writer.write(map, null);
		assertThat(result, is("{\"key\":\"value\"}"));
	}

	@Test
	void writeEmptyList() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new ArrayList<>(), null);
		assertThat(result, is("[]"));
	}

	@Test
	void writeListWithElements() {
		JSONWriter writer = new JSONWriter(null);
		List<Object> list = new ArrayList<>();
		list.add("a");
		list.add("b");
		String result = writer.write(list, null);
		assertThat(result, is("[\"a\",\"b\"]"));
	}

	@Test
	void writeIntArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new int[]{1, 2, 3}, null);
		assertThat(result, is("[1,2,3]"));
	}

	@Test
	void writeStringArray() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(new String[]{"x", "y"}, null);
		assertThat(result, is("[\"x\",\"y\"]"));
	}

	@Test
	void writeClassType() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(String.class, null);
		assertThat(result, is("\"java.lang.String\""));
	}

	@Test
	void writeCharacterObject() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write(Character.valueOf('Z'), null);
		assertThat(result, is("\"Z\""));
	}

	@Test
	void writeResultIsNotNull() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("test", null);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void writeEmptyString() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("", null);
		assertThat(result, is("\"\""));
	}

	@Test
	void writeMapWithNullValue() {
		JSONWriter writer = new JSONWriter(null);
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("k", null);
		String result = writer.write(map, null);
		assertThat(result, is("{\"k\":null}"));
	}

	@Test
	void writeStringWithSlash() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a/b", null);
		assertThat(result, is("\"a\\/b\""));
	}

	@Test
	void writeStringWithFormFeed() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\fb", null);
		assertThat(result, is("\"a\\fb\""));
	}

	@Test
	void writeStringWithBackspace() {
		JSONWriter writer = new JSONWriter(null);
		String result = writer.write("a\bb", null);
		assertThat(result, is("\"a\\bb\""));
	}
}
