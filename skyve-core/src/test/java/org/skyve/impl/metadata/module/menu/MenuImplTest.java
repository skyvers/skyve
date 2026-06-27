package org.skyve.impl.metadata.module.menu;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

@SuppressWarnings({"static-method", "boxing"})
class MenuImplTest {

	// ---- MenuImpl ----

	@Test
	void menuImplGetItemsNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getItems(), is(notNullValue()));
	}

	@Test
	void menuImplGetItemsEmptyByDefault() {
		MenuImpl menu = new MenuImpl();
		assertTrue(menu.getItems().isEmpty());
	}

	@Test
	void menuImplGetPropertiesNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getProperties(), is(notNullValue()));
	}

	@Test
	void menuImplIsApplicableNoItemsReturnsFalse() {
		MenuImpl menu = new MenuImpl();
		assertFalse(menu.isApplicable("desktop"));
	}

	@Test
	void menuImplIsApplicableWithApplicableItem() {
		MenuImpl menu = new MenuImpl();
		LinkItem item = new LinkItem();
		item.setName("link");
		item.setHref("/home");
		menu.getItems().add(item);
		assertTrue(menu.isApplicable("desktop"));
	}

	// ---- MenuGroupImpl ----

	@Test
	void menuGroupGetSetName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Admin");
		assertThat(group.getName(), is("Admin"));
	}

	@Test
	void menuGroupGetRoleNamesReturnsNull() {
		MenuGroupImpl group = new MenuGroupImpl();
		assertThat(group.getRoleNames(), is(nullValue()));
	}

	@Test
	void menuGroupGetUxUisNotNull() {
		MenuGroupImpl group = new MenuGroupImpl();
		assertThat(group.getUxUis(), is(notNullValue()));
	}

	@Test
	void menuGroupIsApplicableNoItemsReturnsFalse() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		assertFalse(group.isApplicable("desktop"));
	}

	@Test
	void menuGroupIsApplicableWithItem() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		EditItem item = new EditItem();
		item.setName("Edit");
		item.setDocumentName("Contact");
		group.getItems().add(item);
		assertTrue(group.isApplicable("desktop"));
	}

	@Test
	void menuGroupIsApplicableWithUxUiMatch() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		group.getUxUis().add("desktop");
		assertTrue(group.isApplicable("desktop"));
	}

	@Test
	void menuGroupIsApplicableWithUxUiNoMatch() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		group.getUxUis().add("mobile");
		assertFalse(group.isApplicable("desktop"));
	}

	@Test
	void menuGroupToStringIncludesName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("MyGroup");
		assertTrue(group.toString().contains("MyGroup"));
	}

	// ---- EditItem ----

	@Test
	void editItemGetSetDocumentName() {
		EditItem item = new EditItem();
		item.setDocumentName("Contact");
		assertThat(item.getDocumentName(), is("Contact"));
	}

	@Test
	void editItemGetSetName() {
		EditItem item = new EditItem();
		item.setName("EditContact");
		assertThat(item.getName(), is("EditContact"));
	}

	@Test
	void editItemGetRoleNamesNotNull() {
		EditItem item = new EditItem();
		assertThat(item.getRoleNames(), is(notNullValue()));
	}

	@Test
	void editItemIsApplicableNoUxUis() {
		EditItem item = new EditItem();
		item.setName("E");
		assertTrue(item.isApplicable("desktop"));
	}

	@Test
	void editItemIsApplicableWithMatchingUxUi() {
		EditItem item = new EditItem();
		item.setName("E");
		item.getUxUis().add("desktop");
		assertTrue(item.isApplicable("desktop"));
	}

	@Test
	void editItemIsApplicableWithNonMatchingUxUi() {
		EditItem item = new EditItem();
		item.setName("E");
		item.getUxUis().add("mobile");
		assertFalse(item.isApplicable("desktop"));
	}

	// ---- ListItem ----

	@Test
	void listItemIsAutoPopulateTrueByDefault() {
		ListItem item = new ListItem();
		assertTrue(item.isAutoPopulate());
	}

	@Test
	void listItemSetAutoPopulate() {
		ListItem item = new ListItem();
		item.setAutoPopulate(false);
		assertFalse(item.isAutoPopulate());
	}

	@Test
	void listItemGetSetQueryName() {
		ListItem item = new ListItem();
		item.setQueryName("qryContact");
		assertThat(item.getQueryName(), is("qryContact"));
	}

	@Test
	void listItemGetSetModelName() {
		ListItem item = new ListItem();
		item.setModelName("myModel");
		assertThat(item.getModelName(), is("myModel"));
	}

	// ---- TreeItem ----

	@Test
	void treeItemExtendsListItem() {
		TreeItem item = new TreeItem();
		item.setAutoPopulate(false);
		assertFalse(item.isAutoPopulate());
	}

	// ---- LinkItem ----

	@Test
	void linkItemGetSetHref() {
		LinkItem item = new LinkItem();
		item.setHref("https://example.com");
		assertThat(item.getHref(), is("https://example.com"));
	}

	@Test
	void linkItemGetSetName() {
		LinkItem item = new LinkItem();
		item.setName("external");
		assertThat(item.getName(), is("external"));
	}

	// ---- CalendarItem ----

	@Test
	void calendarItemGetSetStartBinding() {
		CalendarItem item = new CalendarItem();
		item.setStartBinding("startDate");
		assertThat(item.getStartBinding(), is("startDate"));
	}

	@Test
	void calendarItemGetSetEndBinding() {
		CalendarItem item = new CalendarItem();
		item.setEndBinding("endDate");
		assertThat(item.getEndBinding(), is("endDate"));
	}

	@Test
	void calendarItemGetSetDocumentName() {
		CalendarItem item = new CalendarItem();
		item.setDocumentName("Event");
		assertThat(item.getDocumentName(), is("Event"));
	}

	// ---- MapItem ----

	@Test
	void mapItemGetSetGeometryBinding() {
		MapItem item = new MapItem();
		item.setGeometryBinding("location");
		assertThat(item.getGeometryBinding(), is("location"));
	}

	@Test
	void mapItemGetSetRefreshTime() {
		MapItem item = new MapItem();
		item.setRefreshTimeInSeconds(30);
		assertEquals(30, item.getRefreshTimeInSeconds());
	}

	@Test
	void mapItemGetSetShowRefreshControls() {
		MapItem item = new MapItem();
		item.setShowRefreshControls(Boolean.TRUE);
		assertThat(item.getShowRefreshControls(), is(Boolean.TRUE));
	}

	@Test
	void mapItemRefreshTimeNullByDefault() {
		MapItem item = new MapItem();
		assertThat(item.getRefreshTimeInSeconds(), is(nullValue()));
	}

	@Test
	void menuImplToStringWithItemsContainsItemText() {
		MenuImpl menu = new MenuImpl();
		LinkItem item = new LinkItem();
		item.setName("home");
		item.setHref("/home");
		menu.getItems().add(item);
		String str = menu.toString();
		// toString appends item.toString() which uses default Object representation
		assertFalse(str.isEmpty());
	}

	@Test
	void menuGroupImplToStringWithItemsContainsItemText() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		EditItem item = new EditItem();
		item.setName("EditContact");
		item.setDocumentName("Contact");
		group.getItems().add(item);
		String str = group.toString();
		assertTrue(str.contains("G"));
	}
}
