package org.skyve.domain.app.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.model.document.ModuleDocument;

/**
 * Tests for the default methods of {@link Tag}.
 */
class TagTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class TagDouble implements Tag {
		private static final long serialVersionUID = 1L;
	}

	private enum TestDocument implements ModuleDocument {
		CONTACT;

		@Override
		public String moduleName() {
			return "admin";
		}

		@Override
		public String documentName() {
			return "Contact";
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void countDocumentWithModuleDocumentDelegatesToTheStringForm() {
		Tag tag = mock(TagDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		doReturn(Long.valueOf(7L)).when(tag).countDocument("admin", "Contact");

		assertEquals(7L, tag.countDocument(TestDocument.CONTACT));
	}
}
