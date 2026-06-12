package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class NullTolerantBeanComparatorTest {

	/** Simple POJO used as comparison fixture. */
	public static class NamedItem {
		private String name;

		NamedItem(String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}
	}

	@Test
	void compareAscendingReturnsNegative() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem("apple"), new NamedItem("banana"));
		assertTrue(result < 0, "apple should be less than banana");
	}

	@Test
	void compareDescendingReturnsPositive() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem("zebra"), new NamedItem("apple"));
		assertTrue(result > 0, "zebra should be greater than apple");
	}

	@Test
	void compareEqualValuesReturnsZero() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem("same"), new NamedItem("same"));
		assertEquals(0, result);
	}

	@Test
	void compareFirstNullPropertyReturnsNegative() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem(null), new NamedItem("banana"));
		assertTrue(result < 0, "null name should be ordered before non-null");
	}

	@Test
	void compareSecondNullPropertyReturnsPositive() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem("apple"), new NamedItem(null));
		assertTrue(result > 0, "non-null name should be ordered after null");
	}

	@Test
	void compareBothNullPropertiesReturnsZero() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("name");
		int result = cmp.compare(new NamedItem(null), new NamedItem(null));
		assertEquals(0, result);
	}

	@Test
	void compareWithNullPropertyFallsBackToDirectComparison() {
		// null property means compare the objects directly through the null-tolerant comparator
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator(null);
		int result = cmp.compare("apple", "banana");
		assertTrue(result < 0, "direct string comparison: apple < banana");
	}

	@Test
	void compareInvalidPropertyThrowsRuntimeException() {
		NullTolerantBeanComparator cmp = new NullTolerantBeanComparator("nonExistentProperty");
		NamedItem first = new NamedItem("a");
		NamedItem second = new NamedItem("b");
		assertThrows(RuntimeException.class, () -> cmp.compare(first, second));
	}
}
