package org.skyve.metadata.controller;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ImplicitActionNameTest {

	@Test
	void getLocalisedDisplayNameReturnsNonNullForSave() {
		// Util.i18n with a non-null key returns the key or its translation
		assertNotNull(ImplicitActionName.Save.getLocalisedDisplayName());
	}
}
