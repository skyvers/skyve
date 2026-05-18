package org.skyve.impl.metadata.view.widget.bound.input;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ContentLinkTest {

	@Test
	void showsLabelByDefaultReturnsTrue() {
		assertTrue(new ContentLink().showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new ContentLink().getProperties());
	}

	@Test
	void getParametersReturnsNonNullMap() {
		assertNotNull(new ContentLink().getParameters());
	}

	@Test
	void getLocalisedValueWithNullValueReturnsNull() {
		ContentLink link = new ContentLink();
		// value is null by default, getLocalisedValue() should return null
		assertNull(link.getLocalisedValue());
	}
}
