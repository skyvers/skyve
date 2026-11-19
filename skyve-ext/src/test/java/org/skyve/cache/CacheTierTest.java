package org.skyve.cache;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class CacheTierTest {

	@Test
	void valuesHasThreeEntries() {
		assertEquals(3, CacheTier.values().length);
	}

	@Test
	void valueOfOnHeap() {
		assertEquals(CacheTier.OnHeap, CacheTier.valueOf("OnHeap"));
	}

	@Test
	void valueOfOffHeap() {
		assertEquals(CacheTier.OffHeap, CacheTier.valueOf("OffHeap"));
	}

	@Test
	void valueOfDisk() {
		assertNotNull(CacheTier.valueOf("Disk"));
	}
}
