package org.skyve.impl.metadata.view.container.form;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FormColumnTest {

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new FormColumn().getProperties());
	}
}
