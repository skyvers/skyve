package org.skyve.impl.metadata.repository.document;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class DynamicClassMapTypeTest {

	@Test
	@SuppressWarnings("static-method")
	public void classesListIsInitiallyEmpty() {
		DynamicClassMapType map = new DynamicClassMapType();
		assertNotNull(map.classes);
		assertTrue(map.classes.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void classesListCanAddEntries() {
		DynamicClassMapType map = new DynamicClassMapType();
		DynamicClassMapEntryType entry = new DynamicClassMapEntryType();
		map.classes.add(entry);
		assertTrue(map.classes.size() == 1);
	}
}
