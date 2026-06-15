package org.skyve.impl.metadata.repository.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class DynamicClassMapTypeTest {

	@Test
	@SuppressWarnings("static-method")
	void classesListIsInitiallyEmpty() {
		DynamicClassMapType map = new DynamicClassMapType();
		assertNotNull(map.classes);
		assertTrue(map.classes.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void classesListCanAddEntries() {
		DynamicClassMapType map = new DynamicClassMapType();
		DynamicClassMapEntryType entry = new DynamicClassMapEntryType();
		map.classes.add(entry);
		assertEquals(1, map.classes.size());
	}
}
