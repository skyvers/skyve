package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ContentSignatureTest {

	@Test
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new ContentSignature().showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new ContentSignature().getProperties());
	}
}
