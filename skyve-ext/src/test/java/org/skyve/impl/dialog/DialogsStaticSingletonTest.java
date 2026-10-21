package org.skyve.impl.dialog;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.dialog.Dialogs;

/**
 * Tests the dialogs singleton that startup initialises and tests replace.
 */
@SuppressWarnings("static-method")
class DialogsStaticSingletonTest {
	@AfterEach
	void restoreDefault() {
		DialogsStaticSingleton.setDefault();
	}

	@Test
	void getFailsBeforeStartupHasSetAnImplementation() throws Exception {
		Field instance = DialogsStaticSingleton.class.getDeclaredField("instance");
		instance.setAccessible(true);
		instance.set(null, null);

		IllegalStateException e = assertThrows(IllegalStateException.class, DialogsStaticSingleton::get);
		assertTrue(e.getMessage().contains("not been initialised"), e.getMessage());
	}

	@Test
	void setDefaultInstallsTheRequestBasedImplementation() {
		DialogsStaticSingleton.setDefault();

		assertTrue(DialogsStaticSingleton.get() instanceof DefaultDialogs);
		assertSame(DialogsStaticSingleton.get(), EXT.getDialogs());
	}

	@Test
	void setInstallsAnotherImplementation() {
		Dialogs replacement = mock(Dialogs.class);

		DialogsStaticSingleton.set(replacement);

		assertSame(replacement, EXT.getDialogs());
	}
}
