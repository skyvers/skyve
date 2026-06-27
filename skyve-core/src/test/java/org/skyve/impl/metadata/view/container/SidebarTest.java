package org.skyve.impl.metadata.view.container;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class SidebarTest {

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Sidebar().getVisibleConditionName());
	}
}
