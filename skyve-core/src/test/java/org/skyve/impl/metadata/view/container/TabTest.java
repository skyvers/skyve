package org.skyve.impl.metadata.view.container;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TabTest {

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		assertNull(new Tab().getEnabledConditionName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Tab().getVisibleConditionName());
	}
}
