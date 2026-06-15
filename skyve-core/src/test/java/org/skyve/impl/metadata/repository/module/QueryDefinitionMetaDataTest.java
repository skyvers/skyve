package org.skyve.impl.metadata.repository.module;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class QueryDefinitionMetaDataTest {

	@Test
	void getLocalisedDescriptionWithNullDescriptionReturnsNull() {
		// QueryDefinitionMetaData is abstract; use SQLMetaData as concrete subclass
		SQLMetaData qd = new SQLMetaData();
		assertNull(qd.getLocalisedDescription());
	}
}
