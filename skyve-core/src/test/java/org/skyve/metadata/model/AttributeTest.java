package org.skyve.metadata.model;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Integer;

@SuppressWarnings("static-method")
class AttributeTest {

	@Test
	void getLocalisedRequiredMessageWithNullRequiredMessageReturnsNull() {
		Integer field = new Integer();
		assertNull(field.getLocalisedRequiredMessage());
	}
}
