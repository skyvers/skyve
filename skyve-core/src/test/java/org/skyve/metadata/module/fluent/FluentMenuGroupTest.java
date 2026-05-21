package org.skyve.metadata.module.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Map;
import java.util.TreeMap;
import java.util.Set;

import org.skyve.metadata.module.menu.MenuItem;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.repository.module.GroupMetaData;

@SuppressWarnings("static-method")
class FluentMenuGroupTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentMenuGroup().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		GroupMetaData gmd = new GroupMetaData();
		assertThat(new FluentMenuGroup(gmd).get(), is(gmd));
	}

	@Test
	void nameSetFromParent() {
		assertThat(new FluentMenuGroup().name("myGroup").get().getName(), is("myGroup"));
	}

	// ---- addListItem / findListItem ----

	@Test
	void addListItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addListItem(new FluentListItem().name("list1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findListItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addListItem(new FluentListItem().name("list1"));
		assertThat(g.findListItem("list1"), is(notNullValue()));
	}

	@Test
	void findListItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findListItem("missing"), is(nullValue()));
	}

	// ---- addEditItem / findEditItem ----

	@Test
	void addEditItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addEditItem(new FluentEditItem().name("edit1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findEditItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addEditItem(new FluentEditItem().name("edit1"));
		assertThat(g.findEditItem("edit1"), is(notNullValue()));
	}

	@Test
	void findEditItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findEditItem("missing"), is(nullValue()));
	}

	// ---- addTreeItem / findTreeItem ----

	@Test
	void addTreeItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addTreeItem(new FluentTreeItem().name("tree1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findTreeItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addTreeItem(new FluentTreeItem().name("tree1"));
		assertThat(g.findTreeItem("tree1"), is(notNullValue()));
	}

	@Test
	void findTreeItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findTreeItem("missing"), is(nullValue()));
	}

	// ---- addMapItem / findMapItem ----

	@Test
	void addMapItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addMapItem(new FluentMapItem().name("map1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findMapItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addMapItem(new FluentMapItem().name("map1"));
		assertThat(g.findMapItem("map1"), is(notNullValue()));
	}

	@Test
	void findMapItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findMapItem("missing"), is(nullValue()));
	}

	// ---- addCalendarItem / findCalendarItem ----

	@Test
	void addCalendarItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addCalendarItem(new FluentCalendarItem().name("cal1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findCalendarItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addCalendarItem(new FluentCalendarItem().name("cal1"));
		assertThat(g.findCalendarItem("cal1"), is(notNullValue()));
	}

	@Test
	void findCalendarItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findCalendarItem("missing"), is(nullValue()));
	}

	// ---- addLinkItem / findLinkItem ----

	@Test
	void addLinkItemAddsItem() {
		FluentMenuGroup g = new FluentMenuGroup().addLinkItem(new FluentLinkItem().name("link1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findLinkItemReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addLinkItem(new FluentLinkItem().name("link1"));
		assertThat(g.findLinkItem("link1"), is(notNullValue()));
	}

	@Test
	void findLinkItemReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findLinkItem("missing"), is(nullValue()));
	}

	// ---- addGroup / findGroup ----

	@Test
	void addGroupAddsSubGroup() {
		FluentMenuGroup g = new FluentMenuGroup().addGroup(new FluentMenuGroup().name("sub1"));
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void findGroupReturnsMatch() {
		FluentMenuGroup g = new FluentMenuGroup().addGroup(new FluentMenuGroup().name("sub1"));
		assertThat(g.findGroup("sub1"), is(notNullValue()));
	}

	@Test
	void findGroupReturnsNullWhenMissing() {
		assertThat(new FluentMenuGroup().findGroup("missing"), is(nullValue()));
	}

	// ---- removeMenuAction / clearMenuActions ----

	@Test
	void removeMenuActionRemovesItem() {
		FluentMenuGroup g = new FluentMenuGroup()
				.addListItem(new FluentListItem().name("list1"))
				.addListItem(new FluentListItem().name("list2"));
		assertEquals(2, g.get().getActions().size());
		g.removeMenuAction("list1");
		assertEquals(1, g.get().getActions().size());
	}

	@Test
	void clearMenuActionsRemovesAll() {
		FluentMenuGroup g = new FluentMenuGroup()
				.addListItem(new FluentListItem().name("a"))
				.addEditItem(new FluentEditItem().name("b"));
		assertEquals(2, g.get().getActions().size());
		g.clearMenuActions();
		assertTrue(g.get().getActions().isEmpty());
	}

	// ---- parent: addUxUi / removeUxUi / clearUxUis ----

	@Test
	void addUxUiAddsEntry() {
		FluentMenuGroup g = new FluentMenuGroup().addUxUi("desktop");
		assertEquals(1, g.get().getUxuis().size());
	}

	@Test
	void removeUxUiRemovesEntry() {
		FluentMenuGroup g = new FluentMenuGroup().addUxUi("desktop").addUxUi("phone");
		g.removeUxUi("desktop");
		assertEquals(1, g.get().getUxuis().size());
	}

	@Test
	void clearUxUisClearsAll() {
		FluentMenuGroup g = new FluentMenuGroup().addUxUi("desktop").addUxUi("phone");
		g.clearUxUis();
		assertTrue(g.get().getUxuis().isEmpty());
	}

	// ---- from() branch tests ----

	@Test
	void fromWithEditItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new EditItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithTreeItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new TreeItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithListItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new ListItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithSubGroupCopiesGroup() {
		MenuGroupImpl src = new MenuGroupImpl();
		MenuGroupImpl subGroup = new MenuGroupImpl();
		src.getItems().add(subGroup);
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithMapItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new MapItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithCalendarItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new CalendarItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

	@Test
	void fromWithLinkItemCopiesItem() {
		MenuGroupImpl src = new MenuGroupImpl();
		src.getItems().add(new LinkItem());
		FluentMenuGroup result = new FluentMenuGroup().from(src);
		assertEquals(1, result.get().getActions().size());
	}

        @Test
        void fromWithUnknownItemTypeThrowsIllegalState() {
                MenuGroupImpl src = new MenuGroupImpl();
                src.getItems().add(new MenuItem() {
                        @Override public String getName() { return "unknown"; }
                        @Override public Map<String, String> getProperties() { return new TreeMap<>(); }
                        @Override public Set<String> getRoleNames() { return Set.of(); }
                        @Override public Set<String> getUxUis() { return Set.of(); }
                        @Override public boolean isApplicable(String uxui) { return true; }
                });
                assertThrows(IllegalStateException.class, () -> new FluentMenuGroup().from(src));
        }
}
