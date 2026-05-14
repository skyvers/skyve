package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.CalendarItemMetaData;
import org.skyve.impl.metadata.repository.module.MapItemMetaData;
import org.skyve.impl.metadata.repository.module.TreeItemMetaData;

/**
 * Tests for FluentMapItem, FluentCalendarItem, and FluentTreeItem.
 */
public class FluentMenuItemVariantsTest {

	// --- FluentMapItem ---

	@Test
	@SuppressWarnings("static-method")
	void mapItemDefaultConstructorCreatesInstance() {
		FluentMapItem m = new FluentMapItem();
		assertNotNull(m.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void mapItemWrappingConstructorUsesProvided() {
		MapItemMetaData md = new MapItemMetaData();
		FluentMapItem m = new FluentMapItem(md);
		assertSame(md, m.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void mapItemDocumentNameReturnsSelf() {
		FluentMapItem m = new FluentMapItem();
		FluentMapItem result = m.documentName("TestDoc");
		assertSame(m, result);
		assertEquals("TestDoc", m.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void mapItemQueryNameReturnsSelf() {
		FluentMapItem m = new FluentMapItem();
		FluentMapItem result = m.queryName("TestQuery");
		assertSame(m, result);
		assertEquals("TestQuery", m.get().getQueryName());
	}

	// --- FluentCalendarItem ---

	@Test
	@SuppressWarnings("static-method")
	void calendarItemDefaultConstructorCreatesInstance() {
		FluentCalendarItem c = new FluentCalendarItem();
		assertNotNull(c.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemWrappingConstructorUsesProvided() {
		CalendarItemMetaData md = new CalendarItemMetaData();
		FluentCalendarItem c = new FluentCalendarItem(md);
		assertSame(md, c.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemDocumentNameReturnsSelf() {
		FluentCalendarItem c = new FluentCalendarItem();
		FluentCalendarItem result = c.documentName("TestDoc");
		assertSame(c, result);
		assertEquals("TestDoc", c.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void calendarItemQueryNameReturnsSelf() {
		FluentCalendarItem c = new FluentCalendarItem();
		FluentCalendarItem result = c.queryName("TestQuery");
		assertSame(c, result);
		assertEquals("TestQuery", c.get().getQueryName());
	}

	// --- FluentTreeItem ---

	@Test
	@SuppressWarnings("static-method")
	void treeItemDefaultConstructorCreatesInstance() {
		FluentTreeItem t = new FluentTreeItem();
		assertNotNull(t.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void treeItemWrappingConstructorUsesProvided() {
		TreeItemMetaData md = new TreeItemMetaData();
		FluentTreeItem t = new FluentTreeItem(md);
		assertSame(md, t.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void treeItemDocumentNameReturnsSelf() {
		FluentTreeItem t = new FluentTreeItem();
		FluentTreeItem result = t.documentName("TestDoc");
		assertSame(t, result);
		assertEquals("TestDoc", t.get().getDocumentName());
	}

	@Test
	@SuppressWarnings("static-method")
	void treeItemQueryNameReturnsSelf() {
		FluentTreeItem t = new FluentTreeItem();
		FluentTreeItem result = t.queryName("TestQuery");
		assertSame(t, result);
		assertEquals("TestQuery", t.get().getQueryName());
	}
}
