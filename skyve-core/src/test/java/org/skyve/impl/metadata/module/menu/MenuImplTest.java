package org.skyve.impl.metadata.module.menu;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
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
		assertThat(menu.getItems().isEmpty(), is(true));
	}

	@Test
	void menuImplGetPropertiesNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getProperties(), is(notNullValue()));
	}

	@Test
	void menuImplIsApplicableNoItemsReturnsFalse() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.isApplicable("desktop"), is(false));
	}

	@Test
	void menuImplIsApplicableWithApplicableItem() {
		MenuImpl menu = new MenuImpl();
		LinkItem item = new LinkItem();
		item.setName("link");
		item.setHref("/home");
		menu.getItems().add(item);
		assertThat(menu.isApplicable("desktop"), is(true));
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
		assertThat(group.isApplicable("desktop"), is(false));
	}

	@Test
	void menuGroupIsApplicableWithItem() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		EditItem item = new EditItem();
		item.setName("Edit");
		item.setDocumentName("Contact");
		group.getItems().add(item);
		assertThat(group.isApplicable("desktop"), is(true));
	}

	@Test
	void menuGroupIsApplicableWithUxUiMatch() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		group.getUxUis().add("desktop");
		assertThat(group.isApplicable("desktop"), is(true));
	}

	@Test
	void menuGroupIsApplicableWithUxUiNoMatch() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("G");
		group.getUxUis().add("mobile");
		assertThat(group.isApplicable("desktop"), is(false));
	}

	@Test
	void menuGroupToStringIncludesName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("MyGroup");
		assertThat(group.toString().contains("MyGroup"), is(true));
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
		assertThat(item.isApplicable("desktop"), is(true));
	}

	@Test
	void editItemIsApplicableWithMatchingUxUi() {
		EditItem item = new EditItem();
		item.setName("E");
		item.getUxUis().add("desktop");
		assertThat(item.isApplicable("desktop"), is(true));
	}

	@Test
	void editItemIsApplicableWithNonMatchingUxUi() {
		EditItem item = new EditItem();
		item.setName("E");
		item.getUxUis().add("mobile");
		assertThat(item.isApplicable("desktop"), is(false));
	}

	// ---- ListItem ----

	@Test
	void listItemIsAutoPopulateTrueByDefault() {
		ListItem item = new ListItem();
		assertThat(item.isAutoPopulate(), is(true));
	}

	@Test
	void listItemSetAutoPopulate() {
		ListItem item = new ListItem();
		item.setAutoPopulate(false);
		assertThat(item.isAutoPopulate(), is(false));
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
		assertThat(item.isAutoPopulate(), is(false));
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
		assertThat(item.getRefreshTimeInSeconds(), is(30));
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
}
