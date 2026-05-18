package org.skyve.persistence;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link AutoClosingIterableAdpater}.
 */
@SuppressWarnings({"static-method", "resource"})
class AutoClosingIterableAdpaterTest {

	@Test
	void iteratorReturnsAllElements() {
		List<String> source = Arrays.asList("alpha", "beta", "gamma");
		AutoClosingIterableAdpater<String> adapter = new AutoClosingIterableAdpater<>(source);

		Iterator<String> it = adapter.iterator();
		assertTrue(it.hasNext());
		assertEquals("alpha", it.next());
		assertEquals("beta", it.next());
		assertEquals("gamma", it.next());
		assertFalse(it.hasNext());
	}

	@Test
	void iteratorOverEmptyListHasNoElements() {
		AutoClosingIterableAdpater<Integer> adapter = new AutoClosingIterableAdpater<>(Arrays.asList());
		assertFalse(adapter.iterator().hasNext());
	}

	@Test
	void closeDoesNotThrow() {
		AutoClosingIterableAdpater<String> adapter = new AutoClosingIterableAdpater<>(Arrays.asList("x"));
		assertDoesNotThrow(() -> adapter.close());
	}
}
