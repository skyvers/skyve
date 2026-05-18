package org.skyve.impl.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class CollectionImplTest {

	@Test
	@SuppressWarnings("static-method")
	void setComplexOrderingUpdatesFlag() {
		CollectionImpl col = new CollectionImpl();
		assertFalse(col.isComplexOrdering());
		col.setComplexOrdering(true);
		assertTrue(col.isComplexOrdering());
	}
}
