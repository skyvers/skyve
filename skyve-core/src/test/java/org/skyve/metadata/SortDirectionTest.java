package org.skyve.metadata;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class SortDirectionTest {
	@Test
	@SuppressWarnings("static-method")
	void reverseAscendingReturnsDescending() {
		assertEquals(SortDirection.descending, SortDirection.ascending.reverse());
	}

	@Test
	@SuppressWarnings("static-method")
	void reverseDescendingReturnsAscending() {
		assertEquals(SortDirection.ascending, SortDirection.descending.reverse());
	}

	@Test
	@SuppressWarnings("static-method")
	void valuesContainsAscendingAndDescending() {
		SortDirection[] values = SortDirection.values();
		assertEquals(2, values.length);
	}

	@Test
	@SuppressWarnings("static-method")
	void valueOfAscending() {
		assertEquals(SortDirection.ascending, SortDirection.valueOf("ascending"));
	}
}
