package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ContentImageTest {

	@Test
	void showsLabelByDefaultReturnsTrue() {
		ContentImage widget = new ContentImage();
		assertTrue(widget.showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		ContentImage widget = new ContentImage();
		assertNotNull(widget.getProperties());
	}
}
