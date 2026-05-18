package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.MenuMetaData;

/**
 * Tests for {@link FluentMenu} and {@link FluentMenuGroup} add/find methods.
 */
@SuppressWarnings("static-method")
class FluentMenuTest {

	// ---- FluentMenu — addGroup / findGroup ----

	@Test
	void menuAddGroupAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addGroup(new FluentMenuGroup().name("AdminGroup"));
		assertThat(menu.findGroup("AdminGroup"), is(notNullValue()));
	}

	@Test
	void menuFindGroupReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findGroup("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addEditItem / findEditItem ----

	@Test
	void menuAddEditItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addEditItem(new FluentEditItem().name("EditContact"));
		assertThat(menu.findEditItem("EditContact"), is(notNullValue()));
	}

	@Test
	void menuFindEditItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findEditItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addListItem / findListItem ----

	@Test
	void menuAddListItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addListItem(new FluentListItem().name("ListContacts"));
		assertThat(menu.findListItem("ListContacts"), is(notNullValue()));
	}

	@Test
	void menuFindListItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findListItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addCalendarItem / findCalendarItem ----

	@Test
	void menuAddCalendarItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addCalendarItem(new FluentCalendarItem().name("CalendarEvents"));
		assertThat(menu.findCalendarItem("CalendarEvents"), is(notNullValue()));
	}

	@Test
	void menuFindCalendarItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findCalendarItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addLinkItem / findLinkItem ----

	@Test
	void menuAddLinkItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addLinkItem(new FluentLinkItem().name("ExternalLink"));
		assertThat(menu.findLinkItem("ExternalLink"), is(notNullValue()));
	}

	@Test
	void menuFindLinkItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findLinkItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addMapItem / findMapItem ----

	@Test
	void menuAddMapItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addMapItem(new FluentMapItem().name("MapContacts"));
		assertThat(menu.findMapItem("MapContacts"), is(notNullValue()));
	}

	@Test
	void menuFindMapItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findMapItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — addTreeItem / findTreeItem ----

	@Test
	void menuAddTreeItemAndFindByName() {
		FluentMenu menu = new FluentMenu();
		menu.addTreeItem(new FluentTreeItem().name("TreeContacts"));
		assertThat(menu.findTreeItem("TreeContacts"), is(notNullValue()));
	}

	@Test
	void menuFindTreeItemReturnsNullWhenNotFound() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.findTreeItem("missing"), is(nullValue()));
	}

	// ---- FluentMenu — removeMenuAction / clearMenuActions ----

	@Test
	void menuRemoveMenuActionByName() {
		FluentMenu menu = new FluentMenu();
		menu.addEditItem(new FluentEditItem().name("EditContact"));
		menu.addListItem(new FluentListItem().name("ListContacts"));
		menu.removeMenuAction("EditContact");
		assertEquals(1, menu.get().getActions().size());
		assertThat(menu.findEditItem("EditContact"), is(nullValue()));
	}

	@Test
	void menuClearMenuActionsRemovesAll() {
		FluentMenu menu = new FluentMenu();
		menu.addEditItem(new FluentEditItem().name("EditContact"));
		menu.addListItem(new FluentListItem().name("ListContacts"));
		menu.addTreeItem(new FluentTreeItem().name("TreeView"));
		menu.clearMenuActions();
		assertEquals(0, menu.get().getActions().size());
	}

	// ---- FluentMenu — addAction (via FluentMenuGroup) ----

	@Test
	void menuAddActionViaGroupProxy() {
		FluentMenu menu = new FluentMenu();
		menu.addGroup(new FluentMenuGroup().name("MyGroup"));
		assertEquals(1, menu.get().getActions().size());
	}

	// ---- FluentMenuGroup — addGroup / findGroup ----

	@Test
	void menuGroupAddSubGroupAndFindByName() {
		FluentMenuGroup parent = new FluentMenuGroup().name("Parent");
		parent.addGroup(new FluentMenuGroup().name("Child"));
		assertThat(parent.findGroup("Child"), is(notNullValue()));
	}

	@Test
	void menuGroupFindGroupReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findGroup("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addEditItem / findEditItem ----

	@Test
	void menuGroupAddEditItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addEditItem(new FluentEditItem().name("EditUser"));
		assertThat(group.findEditItem("EditUser"), is(notNullValue()));
	}

	@Test
	void menuGroupFindEditItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findEditItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addListItem / findListItem ----

	@Test
	void menuGroupAddListItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addListItem(new FluentListItem().name("ListUsers"));
		assertThat(group.findListItem("ListUsers"), is(notNullValue()));
	}

	@Test
	void menuGroupFindListItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findListItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addCalendarItem / findCalendarItem ----

	@Test
	void menuGroupAddCalendarItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addCalendarItem(new FluentCalendarItem().name("CalEvents"));
		assertThat(group.findCalendarItem("CalEvents"), is(notNullValue()));
	}

	@Test
	void menuGroupFindCalendarItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findCalendarItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addLinkItem / findLinkItem ----

	@Test
	void menuGroupAddLinkItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addLinkItem(new FluentLinkItem().name("ExtLink"));
		assertThat(group.findLinkItem("ExtLink"), is(notNullValue()));
	}

	@Test
	void menuGroupFindLinkItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findLinkItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addMapItem / findMapItem ----

	@Test
	void menuGroupAddMapItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addMapItem(new FluentMapItem().name("MapUsers"));
		assertThat(group.findMapItem("MapUsers"), is(notNullValue()));
	}

	@Test
	void menuGroupFindMapItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findMapItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — addTreeItem / findTreeItem ----

	@Test
	void menuGroupAddTreeItemAndFindByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addTreeItem(new FluentTreeItem().name("TreeUsers"));
		assertThat(group.findTreeItem("TreeUsers"), is(notNullValue()));
	}

	@Test
	void menuGroupFindTreeItemReturnsNullWhenNotFound() {
		FluentMenuGroup group = new FluentMenuGroup();
		assertThat(group.findTreeItem("missing"), is(nullValue()));
	}

	// ---- FluentMenuGroup — removeMenuAction / clearMenuActions ----

	@Test
	void menuGroupRemoveMenuActionByName() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addEditItem(new FluentEditItem().name("EditUser"));
		group.addListItem(new FluentListItem().name("ListUsers"));
		group.removeMenuAction("EditUser");
		assertEquals(1, group.get().getActions().size());
		assertThat(group.findEditItem("EditUser"), is(nullValue()));
	}

	@Test
	void menuGroupClearMenuActionsRemovesAll() {
		FluentMenuGroup group = new FluentMenuGroup();
		group.addEditItem(new FluentEditItem().name("EditUser"));
		group.addListItem(new FluentListItem().name("ListUsers"));
		group.clearMenuActions();
		assertEquals(0, group.get().getActions().size());
	}

	// ---- FluentMenuAction — name / addUxUi / removeUxUi / clearUxUis ----

	@Test
	void menuActionNameSetsValue() {
		FluentEditItem item = new FluentEditItem().name("TestItem");
		assertThat(item.get().getName(), is("TestItem"));
	}

	@Test
	void menuActionAddUxUiAddsEntry() {
		FluentEditItem item = new FluentEditItem().name("TestItem");
		item.addUxUi("desktop");
		assertEquals(1, item.get().getUxuis().size());
	}

	@Test
	void menuActionRemoveUxUiRemovesEntry() {
		FluentEditItem item = new FluentEditItem().name("TestItem");
		item.addUxUi("desktop");
		item.addUxUi("mobile");
		item.removeUxUi("desktop");
		assertEquals(1, item.get().getUxuis().size());
	}

	@Test
	void menuActionClearUxUisRemovesAll() {
		FluentEditItem item = new FluentEditItem().name("TestItem");
		item.addUxUi("desktop");
		item.addUxUi("mobile");
		item.clearUxUis();
		assertEquals(0, item.get().getUxuis().size());
	}

	@Test
	void menuFromCopiesEditItemsAndListItems() {
		MenuImpl source = new MenuImpl();
		EditItem edit = new EditItem();
		edit.setName("EditContact");
		edit.setDocumentName("Contact");
		source.getItems().add(edit);

		ListItem list = new ListItem();
		list.setName("AllContacts");
		list.setDocumentName("Contact");
		list.setQueryName("qContacts");
		source.getItems().add(list);

		FluentMenu menu = new FluentMenu().from(source);
		assertThat(menu.findEditItem("EditContact"), is(notNullValue()));
		assertThat(menu.findListItem("AllContacts"), is(notNullValue()));
	}

	@Test
	void menuFromCopiesTreeItemAndCalendarItem() {
		MenuImpl source = new MenuImpl();
		TreeItem tree = new TreeItem();
		tree.setName("TreeItems");
		tree.setDocumentName("Category");
		tree.setQueryName("qCategories");
		source.getItems().add(tree);

		CalendarItem calendar = new CalendarItem();
		calendar.setName("CalItems");
		calendar.setDocumentName("Event");
		calendar.setQueryName("qEvents");
		source.getItems().add(calendar);

		FluentMenu menu = new FluentMenu().from(source);
		assertThat(menu.findTreeItem("TreeItems"), is(notNullValue()));
		assertThat(menu.findCalendarItem("CalItems"), is(notNullValue()));
	}

	@Test
	void menuFromCopiesMapItemAndLinkItem() {
		MenuImpl source = new MenuImpl();
		MapItem map = new MapItem();
		map.setName("MapView");
		map.setDocumentName("Location");
		map.setQueryName("qLocations");
		source.getItems().add(map);

		LinkItem link = new LinkItem();
		link.setName("ExternalSite");
		link.setHref("https://example.com");
		source.getItems().add(link);

		FluentMenu menu = new FluentMenu().from(source);
		assertThat(menu.findMapItem("MapView"), is(notNullValue()));
		assertThat(menu.findLinkItem("ExternalSite"), is(notNullValue()));
	}

	@Test
	void menuFromCopiesMenuGroup() {
		MenuImpl source = new MenuImpl();
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("AdminGroup");
		source.getItems().add(group);

		FluentMenu menu = new FluentMenu().from(source);
		assertThat(menu.findGroup("AdminGroup"), is(notNullValue()));
	}

	@Test
	void menuWrappingConstructorPreservesInstance() {
		MenuMetaData existing = new MenuMetaData();
		FluentMenu fluent = new FluentMenu(existing);
		assertSame(existing, fluent.get());
	}
}
