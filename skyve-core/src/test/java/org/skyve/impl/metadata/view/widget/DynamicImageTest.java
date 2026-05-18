package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DynamicImageTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		DynamicImage widget = new DynamicImage();
		assertNull(widget.getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		DynamicImage widget = new DynamicImage();
		assertNotNull(widget.getProperties());
	}
}
