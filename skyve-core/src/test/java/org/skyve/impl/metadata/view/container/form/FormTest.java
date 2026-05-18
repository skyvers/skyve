package org.skyve.impl.metadata.view.container.form;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FormTest {

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		assertNull(new Form().getEnabledConditionName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new Form().getVisibleConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Form().getProperties());
	}
}
