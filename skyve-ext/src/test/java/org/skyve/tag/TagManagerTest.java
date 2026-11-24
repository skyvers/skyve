package org.skyve.tag;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Tests that the typed-reference default methods of {@link TagManager} delegate to the String
 * forms with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class TagManagerTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class TagManagerDouble implements TagManager {
		// no state
	}

	@Test
	void typedTagAndUntagDelegateToTheStringForms() throws Exception {
		TagManager manager = mock(TagManagerDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));

		manager.tag("tag1", SalesDocument.ORDER, "order1");
		manager.untag("tag1", SalesDocument.ORDER, "order2");

		assertAll(() -> verify(manager).tag("tag1", "sales", "Order", "order1"),
					() -> verify(manager).untag("tag1", "sales", "Order", "order2"));
	}

	private enum SalesDocument implements ModuleDocument {
		ORDER;

		@Override
		public String moduleName() {
			return "sales";
		}

		@Override
		public String documentName() {
			return "Order";
		}
	}
}
