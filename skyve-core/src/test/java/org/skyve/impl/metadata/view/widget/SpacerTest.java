package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SpacerTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Spacer().showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Spacer().getProperties());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Spacer().getVisibleConditionName());
	}
}
