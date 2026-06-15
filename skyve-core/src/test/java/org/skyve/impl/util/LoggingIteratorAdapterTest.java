package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class LoggingIteratorAdapterTest {

	@Test
	void hasNextReturnsTrueWhenElementsRemain() {
		List<String> items = Arrays.asList("a", "b", "c");
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		assertTrue(adapter.hasNext());
	}

	@Test
	void hasNextReturnsFalseWhenNoElementsRemain() {
		List<String> items = Collections.emptyList();
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		assertFalse(adapter.hasNext());
	}

	@Test
	void nextReturnsCorrectElements() {
		List<String> items = Arrays.asList("x", "y");
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		assertEquals("x", adapter.next());
		assertEquals("y", adapter.next());
	}

	@Test
	void iteratesAllElementsInOrder() {
		List<String> items = Arrays.asList("a1", "b2", "c3", "d4", "e5");
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		int index = 0;
		while (adapter.hasNext()) {
			assertEquals(items.get(index), adapter.next());
			index++;
		}
		assertEquals(5, index);
	}

	@Test
	void hasNextReturnsFalseAfterAllElementsConsumed() {
		List<String> items = Arrays.asList("a");
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		adapter.next();
		assertFalse(adapter.hasNext());
	}

	@Test
	void wrapsArbitraryIterator() {
		List<String> source = Arrays.asList("one", "two", "three");
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(source.iterator());
		StringBuilder sb = new StringBuilder();
		while (adapter.hasNext()) {
			sb.append(adapter.next());
		}
		assertEquals("onetwothree", sb.toString());
	}

	@Test
	void iterates1000ElementsTriggersIteratedLogging() {
		// Calling next() 1000 times triggers the %1000==0 log branch
		List<String> items = new java.util.ArrayList<>(1000);
		for (int i = 0; i < 1000; i++) {
			items.add("item" + i);
		}
		LoggingIteratorAdapter<String> adapter = new LoggingIteratorAdapter<>(items.iterator());
		int count = 0;
		while (adapter.hasNext()) {
			adapter.next();
			count++;
		}
		assertEquals(1000, count);
		// After draining 1000 items, hasNext() returns false and iterated==1000 → triggers second log
		assertFalse(adapter.hasNext());
	}
}
