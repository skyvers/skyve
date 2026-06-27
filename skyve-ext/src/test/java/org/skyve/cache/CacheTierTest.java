package org.skyve.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

@SuppressWarnings("static-method")
public class CacheTierTest {

	@Test
	public void valuesHasThreeEntries() {
		assertEquals(3, CacheTier.values().length);
	}

	@Test
	public void valueOfOnHeap() {
		assertEquals(CacheTier.OnHeap, CacheTier.valueOf("OnHeap"));
	}

	@Test
	public void valueOfOffHeap() {
		assertEquals(CacheTier.OffHeap, CacheTier.valueOf("OffHeap"));
	}

	@Test
	public void valueOfDisk() {
		assertNotNull(CacheTier.valueOf("Disk"));
	}
}
