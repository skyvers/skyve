package org.skyve.metadata.user;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

/**
 * Tests that the typed-reference default methods of {@link User} delegate to the String forms
 * with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class UserTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class UserDouble implements User {
		private static final long serialVersionUID = 1L;
	}

	@Test
	void typedBeanAndContentChecksDelegateToTheStringForms() {
		User user = mock(UserDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		doReturn(Boolean.TRUE).when(user).canReadBean("id1", "admin", "Contact", "acme", "dg1", "u1");
		doReturn(Boolean.FALSE).when(user).canReadBean("id2", "admin", "Contact", "acme", null, null);
		doReturn(Boolean.TRUE).when(user).canAccessContent("id1", "admin", "Contact", "acme", "dg1", "u1", "image");
		doReturn(Boolean.FALSE).when(user).canAccessContent("id2", "admin", "Contact", "acme", null, null, "image");

		assertAll(() -> assertTrue(user.canReadBean("id1", TestDocument.CONTACT, "acme", "dg1", "u1")),
					() -> assertFalse(user.canReadBean("id2", TestDocument.CONTACT, "acme", null, null)),
					() -> assertTrue(user.canAccessContent("id1", TestDocument.CONTACT, "acme", "dg1", "u1", "image")),
					() -> assertFalse(user.canAccessContent("id2", TestDocument.CONTACT, "acme", null, null, "image")));
	}
}
