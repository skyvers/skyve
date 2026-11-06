package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Tests the capture heuristic: a value the application showed that a later step reuses becomes a capture where it
 * was shown and a {@code {var:name}} where it is reused; anything else is left alone.
 */
@SuppressWarnings("static-method")
class SailRecordingCapturesTest {
	private static Map<String, String> step(String... keysAndValues) {
		Map<String, String> result = new LinkedHashMap<>();
		for (int i = 0; i < keysAndValues.length; i += 2) {
			result.put(keysAndValues[i], keysAndValues[i + 1]);
		}
		return result;
	}

	@Test
	void aShownValueReusedLaterIsCapturedWhereItWasShown() {
		List<Map<String, String>> result = SailRecordingCaptures.apply(List.of(
				step("op", "dataEnter", "binding", "description", "value", "Widget"),
				step("op", "save"),
				step("op", "context", "text", "changed on screen: number", "shown", "{\"number\":\"108\",\"description\":\"Widget\"}"),
				step("op", "ok"),
				step("op", "listGridFilter", "column", "number", "value", "108", "operator", "equals"),
				step("op", "listGridZoom", "rowContaining", "108")));
		assertAll(() -> assertEquals(7, result.size(), result.toString()),
					() -> assertEquals("captureValue", result.get(3).get("op")),
					() -> assertEquals("number", result.get(3).get("binding")),
					() -> assertEquals("number", result.get(3).get("variable")),
					() -> assertEquals("{var:number}", result.get(5).get("value")),
					() -> assertEquals("{var:number}", result.get(6).get("rowContaining")));
	}

	@Test
	void whatTheUserTypedOrTrivialValuesAreNotCaptured() {
		List<Map<String, String>> entries = List.of(
				step("op", "dataEnter", "binding", "description", "value", "Widget"),
				step("op", "context", "shown", "{\"description\":\"Widget\",\"flag\":\"true\",\"code\":\"ab\"}"),
				step("op", "dataEnter", "binding", "other", "value", "Widget"),
				step("op", "dataEnter", "binding", "flag2", "value", "true"),
				step("op", "dataEnter", "binding", "code2", "value", "ab"));
		List<Map<String, String>> result = SailRecordingCaptures.apply(entries);
		assertEquals(entries, result);
	}

	@Test
	void cellsAndTitlesAreCapturedBeforeTheirStep() {
		List<Map<String, String>> result = SailRecordingCaptures.apply(List.of(
				step("op", "dataGridZoom", "binding", "rows", "rowContaining", "Fred", "cells", "{\"name\":\"Fred\",\"code\":\"X-17\"}",
						"title", "Order 2024-0042"),
				step("op", "zoomOut"),
				step("op", "dataEnter", "binding", "reference", "value", "X-17"),
				step("op", "dataEnter", "binding", "note", "value", "Order 2024-0042")));
		Map<String, String> cell = result.get(0);
		Map<String, String> title = result.get(1);
		assertAll(() -> assertEquals(6, result.size(), result.toString()),
					() -> assertEquals("captureCell", cell.get("op")),
					() -> assertEquals("rows", cell.get("binding")),
					() -> assertEquals("Fred", cell.get("rowContaining")),
					() -> assertEquals("code", cell.get("column")),
					() -> assertEquals("captureTitle", title.get("op")),
					() -> assertEquals("title", title.get("variable")),
					() -> assertEquals("{var:code}", result.get(4).get("value")),
					() -> assertEquals("{var:title}", result.get(5).get("value")));
	}

	@Test
	void namesAreUniquePerValue() {
		List<Map<String, String>> result = SailRecordingCaptures.apply(List.of(
				step("op", "context", "shown", "{\"a.number\":\"101\"}"),
				step("op", "context", "shown", "{\"b.number\":\"202\"}"),
				step("op", "dataEnter", "binding", "x", "value", "101"),
				step("op", "dataEnter", "binding", "y", "value", "202")));
		String xml = result.toString();
		assertAll(() -> assertTrue(xml.contains("variable=number,") || xml.contains("variable=number}"), xml),
					() -> assertTrue(xml.contains("variable=number2"), xml),
					() -> assertEquals("{var:number}", result.get(4).get("value")),
					() -> assertEquals("{var:number2}", result.get(5).get("value")));
	}

	/** The built script has the capture, and the checks leave a captured row alone. */
	@Test
	void theScriptCapturesAndReuses() {
		String xml = SailRecordingBuilder.toXml(new SailRecordingBuilder(null, "admin", 0L, 0L).build(List.of(
				step("op", "save", "view", "kitchensink.LookupDescription"),
				step("op", "context", "text", "changed on screen: number", "shown", "{\"number\":\"108\"}"),
				step("op", "ok"),
				step("op", "listGridZoom", "rowContaining", "108", "rowTexts", "Widget | 108"))));
		assertAll(() -> assertTrue(xml.contains("<captureValue binding=\"number\" variable=\"number\"/>"), xml),
					() -> assertTrue(xml.contains("<listGridZoom rowContaining=\"{var:number}\"/>"), xml));
	}
}
