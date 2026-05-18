package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.NoSuchElementException;

import org.junit.Test;

import jakarta.faces.model.SelectItem;

@SuppressWarnings("static-method")
public class SelectItemsBeanConverterArrayIteratorTest {

	@Test
	public void hasNextReturnsFalseForEmptyArray() {
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[0]);
		assertFalse(it.hasNext());
	}

	@Test
	public void hasNextReturnsTrueForNonEmptyArray() {
		SelectItem item = new SelectItem("value", "label");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		assertTrue(it.hasNext());
	}

	@Test
	public void nextReturnsSingleItem() {
		SelectItem item = new SelectItem("value", "label");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		assertEquals(item, it.next());
		assertFalse(it.hasNext());
	}

	@Test
	public void nextIteratesAllItems() {
		SelectItem a = new SelectItem("a", "A");
		SelectItem b = new SelectItem("b", "B");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{a, b});
		assertEquals(a, it.next());
		assertEquals(b, it.next());
		assertFalse(it.hasNext());
	}

	@Test(expected = NoSuchElementException.class)
	public void nextThrowsWhenExhausted() {
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[0]);
		it.next();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void removeThrowsUnsupportedOperation() {
		SelectItem item = new SelectItem("v", "l");
		SelectItemsBeanConverter.ArrayIterator it = new SelectItemsBeanConverter.ArrayIterator(new SelectItem[]{item});
		it.remove();
	}
}
