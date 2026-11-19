package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.NoSuchElementException;

import org.junit.jupiter.api.Test;

import jakarta.faces.model.SelectItem;
@SuppressWarnings("static-method")
class SelectItemsBeanConverterArrayIteratorTest {

	@Test
	void hasNextReturnsFalseForEmptyArray() {
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[0]);
		assertFalse(it.hasNext());
	}

	@Test
	void hasNextReturnsTrueForNonEmptyArray() {
		SelectItem item = new SelectItem("value", "label");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		assertTrue(it.hasNext());
	}

	@Test
	void nextReturnsSingleItem() {
		SelectItem item = new SelectItem("value", "label");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		assertEquals(item, it.next());
		assertFalse(it.hasNext());
	}

	@Test
	void nextIteratesAllItems() {
		SelectItem a = new SelectItem("a", "A");
		SelectItem b = new SelectItem("b", "B");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{a, b});
		assertEquals(a, it.next());
		assertEquals(b, it.next());
		assertFalse(it.hasNext());
	}

	@Test
	void nextThrowsWhenExhausted() {
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[0]);
		assertThrows(NoSuchElementException.class, it::next);
	}

	@Test
	void removeThrowsUnsupportedOperation() {
		SelectItem item = new SelectItem("v", "l");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		assertThrows(UnsupportedOperationException.class, it::remove);
	}
}
