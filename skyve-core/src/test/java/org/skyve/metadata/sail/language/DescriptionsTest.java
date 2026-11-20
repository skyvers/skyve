package org.skyve.metadata.sail.language;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DescriptionsTest {
	@Test
	void noTextIsNoDescription() {
		assertAll(() -> assertNull(Descriptions.normalise(null)),
					() -> assertNull(Descriptions.normalise("")),
					() -> assertNull(Descriptions.normalise(" \n\t\n  ")));
	}

	@Test
	void lineBreaksAndIndentsInAParagraphAreSpaces() {
		assertEquals("Signs in, adds the line and submits.", Descriptions.normalise("\n\t\tSigns in,  adds the line\n\t\tand\tsubmits.\n\t"));
	}

	@Test
	void aBlankLineStartsAParagraph() {
		assertAll(() -> assertEquals("One.\n\nTwo, still two.\n\nThree.", Descriptions.normalise("One.\n\n\tTwo,\n\tstill two.\n \t \n\n\tThree.")),
					() -> assertEquals("One.\n\nTwo.", Descriptions.normalise("One.\r\n\r\nTwo.")),
					() -> assertEquals("One. Two.", Descriptions.normalise("One.\r\nTwo.")),
					() -> assertEquals("Ben claims, and Rob approves.", Descriptions.normalise("Ben claims,\r\n\t\tand Rob approves.")),
					() -> assertEquals("One. Two.", Descriptions.normalise("One.\rTwo.")));
	}

	@Test
	void aNoBreakSpaceIsASpace() {
		assertAll(() -> assertEquals("One.\n\nTwo.", Descriptions.normalise("One.\n\u00A0\n\nTwo.")),
					() -> assertEquals("One.", Descriptions.normalise("One.\n\u00A0")),
					() -> assertEquals("One two.", Descriptions.normalise("One\u00A0\u00A0two.")));
	}

	@Test
	void tokensAreKeptAsWritten() {
		assertEquals("For {run} and {var:claim}.", Descriptions.normalise("For {run} and {var:claim}."));
	}

	@Test
	void paragraphsSplitATidiedDescription() {
		assertAll(() -> assertEquals(List.of(), Descriptions.paragraphs(null)),
					() -> assertEquals(List.of("One."), Descriptions.paragraphs("One.")),
					() -> assertEquals(List.of("One.", "Two."), Descriptions.paragraphs(Descriptions.normalise("One.\n\n  Two."))));
	}
}
