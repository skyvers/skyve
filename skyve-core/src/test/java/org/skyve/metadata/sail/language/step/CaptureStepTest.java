package org.skyve.metadata.sail.language.step;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class CaptureStepTest {

	@Test
	void extractKeepsTheTextWithoutAPattern() {
		assertEquals("Request 42 - printer", CaptureStep.extract(null, "Request 42 - printer"));
	}

	@Test
	void extractTakesTheFirstGroupOfTheFirstMatch() {
		assertAll(() -> assertEquals("42", CaptureStep.extract("Request (\\d+)", "Request 42 - Request 43")),
					() -> assertEquals("42", CaptureStep.extract("(\\d+)-(\\d+)", "x 42-43")));
	}

	@Test
	void extractTakesTheWholeMatchWithoutAGroup() {
		assertEquals("REF-42", CaptureStep.extract("REF-\\d+", "Your ref is REF-42."));
	}

	@Test
	void extractReturnsNullWhenThePatternDoesNotMatch() {
		assertAll(() -> assertNull(CaptureStep.extract("REF-(\\d+)", "no reference")),
					// an optional group that took no part in the match
					() -> assertNull(CaptureStep.extract("REF(-\\d+)?", "REF")));
	}
}
