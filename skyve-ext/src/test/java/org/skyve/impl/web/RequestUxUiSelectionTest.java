package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.router.UxUi;
import org.skyve.web.UserAgentType;

@SuppressWarnings("static-method")
class RequestUxUiSelectionTest {
	@Test
	void partialSelectionCompletesExactlyOnce() {
		RequestUxUiSelection selection = new RequestUxUiSelection(UserAgentType.tablet, true);
		UxUi uxui = UxUi.newPrimeFaces("tablet", "external", "tablet-theme");

		assertSame(UserAgentType.tablet, selection.getUserAgentType());
		assertTrue(selection.isEmulated());
		assertFalse(selection.isComplete());
		assertThrows(IllegalStateException.class, selection::getUxUi);

		selection.complete(uxui);

		assertTrue(selection.isComplete());
		assertSame(uxui, selection.getUxUi());
		assertThrows(IllegalStateException.class, () -> selection.complete(uxui));
	}
}
