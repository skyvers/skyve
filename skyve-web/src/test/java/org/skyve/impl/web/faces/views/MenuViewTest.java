package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.menu.MenuItem;

@SuppressWarnings("static-method")
class MenuViewTest {
	@Test
	void createMenuHrefUsesAbsoluteHrefWhenProvidedAndEscapesSingleQuotes() {
		MenuItem item = mock(MenuItem.class);

		String href = MenuView.createMenuHref(module("menu"), module("item"), item, "ignored", "/external/it's");

		assertEquals("javascript:SKYVE.PF.startHistory('/external/it\\'s')", href);
	}

	@Test
	void createMenuHrefBuildsListHrefForNamedModel() {
		ListItem item = new ListItem();
		item.setDocumentName("Contact");
		item.setModelName("activeContacts");

		String href = MenuView.createMenuHref(module("sales"), null, item, "ignoredQuery", null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=l&m=sales&d=Contact&q=activeContacts"));
	}

	@Test
	void createMenuHrefBuildsListHrefForResolvedQuery() {
		ListItem item = new ListItem();

		String href = MenuView.createMenuHref(module("sales"), null, item, "allContacts", null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=l&m=sales&q=allContacts"));
	}

	@Test
	void createMenuHrefBuildsEditHrefForTargetModuleAndDocument() {
		EditItem item = new EditItem();
		item.setDocumentName("Contact");

		String href = MenuView.createMenuHref(module("menu"), module("crm"), item, null, null);

		assertHasStartHistoryWrapper(href);
		assertTrue(href.contains("/?a=e&m=crm&d=Contact"));
	}

	@Test
	void createMenuHrefBuildsCalendarAndMapHrefs() {
		CalendarItem calendar = new CalendarItem();
		MapItem map = new MapItem();
		map.setDocumentName("Location");
		map.setModelName("siteMap");
		map.setGeometryBinding("location");

		String calendarHref = MenuView.createMenuHref(module("sales"), null, calendar, "events", null);
		String mapHref = MenuView.createMenuHref(module("sales"), null, map, "ignored", null);

		assertTrue(calendarHref.contains("/?a=c&m=sales&q=events"));
		assertTrue(mapHref.contains("/?a=m&m=sales&d=Location&q=siteMap&b=location"));
	}

	private static Module module(String name) {
		Module module = mock(Module.class);
		when(module.getName()).thenReturn(name);
		return module;
	}

	private static void assertHasStartHistoryWrapper(String href) {
		assertTrue(href.startsWith("javascript:SKYVE.PF.startHistory('"));
		assertTrue(href.endsWith("')"));
	}
}
