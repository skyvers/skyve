package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class LinkTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Link().showsLabelByDefault());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Link().getVisibleConditionName());
	}
}
