package org.skyve.impl.metadata.repository.module;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ContentPermissionTest {

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new ContentPermission().getProperties());
	}
}
