package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class BlurbTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Blurb().showsLabelByDefault());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Blurb().getVisibleConditionName());
	}
}
