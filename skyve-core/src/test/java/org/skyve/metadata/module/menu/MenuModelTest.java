package org.skyve.metadata.module.menu;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;

@SuppressWarnings("static-method")
class MenuModelTest {

	// --- MenuImpl ---

	@Test
	void menuImplItemsNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getItems(), notNullValue());
	}

	@Test
	void menuImplItemsEmptyByDefault() {
		MenuImpl menu = new MenuImpl();
		assertTrue(menu.getItems().isEmpty());
	}

	@Test
	void menuImplPropertiesNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getProperties(), notNullValue());
	}

	@Test
	void menuImplIsApplicableReturnsFalseWhenNoItems() {
		MenuImpl menu = new MenuImpl();
		assertFalse(menu.isApplicable("desktop"));
	}

	@Test
	void menuImplIsApplicableReturnsTrueWhenItemIsApplicable() {
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("testItem");
		// empty uxuis means applicable to all
		menu.getItems().add(item);
		assertTrue(menu.isApplicable("desktop"));
	}

	@Test
	void menuImplIsApplicableReturnsFalseWhenItemNotApplicable() {
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("testItem");
		item.getUxUis().add("mobile");
		menu.getItems().add(item);
		assertFalse(menu.isApplicable("desktop"));
	}

	@Test
	void menuImplToStringNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.toString(), notNullValue());
	}

	// --- MenuGroupImpl ---

	@Test
	void menuGroupImplNameNullByDefault() {
		MenuGroupImpl group = new MenuGroupImpl();
		assertThat(group.getName(), is((String) null));
	}

	@Test
	void menuGroupImplSetAndGetName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Admin");
		assertThat(group.getName(), is("Admin"));
	}

	@Test
	void menuGroupImplRoleNamesReturnsNull() {
		// MenuGroupImpl.getRoleNames() returns null by design
		MenuGroupImpl group = new MenuGroupImpl();
		assertThat(group.getRoleNames(), is((Object) null));
	}

	@Test
	void menuGroupImplUxUisNotNull() {
		MenuGroupImpl group = new MenuGroupImpl();
		assertThat(group.getUxUis(), notNullValue());
	}

	@Test
	void menuGroupImplIsApplicableWithEmptyUxUis() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Test");
		EditItem item = new EditItem();
		item.setName("item");
		group.getItems().add(item);
		// group has empty uxuis, but its applicability depends on its items
		assertTrue(group.isApplicable("desktop"));
	}

	@Test
	void menuGroupImplToStringContainsName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("MyGroup");
		assertTrue(group.toString().contains("MyGroup"));
	}

	@Test
	void menuGroupImplIsApplicableReturnsFalseWhenUxUiNotMatched() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Mobile");
		group.getUxUis().add("mobile");
		assertFalse(group.isApplicable("desktop"));
	}

	// --- EditItem ---

	@Test
	void editItemNameNullByDefault() {
		EditItem item = new EditItem();
		assertThat(item.getName(), is((String) null));
	}

	@Test
	void editItemSetAndGetName() {
		EditItem item = new EditItem();
		item.setName("editContact");
		assertThat(item.getName(), is("editContact"));
	}

	@Test
	void editItemRoleNamesNotNull() {
		EditItem item = new EditItem();
		assertThat(item.getRoleNames(), notNullValue());
	}

	@Test
	void editItemUxUisNotNull() {
		EditItem item = new EditItem();
		assertThat(item.getUxUis(), notNullValue());
	}

	@Test
	void editItemIsApplicableWithEmptyUxUis() {
		EditItem item = new EditItem();
		assertTrue(item.isApplicable("desktop"));
	}

	@Test
	void editItemIsApplicableWithMatchingUxUi() {
		EditItem item = new EditItem();
		item.getUxUis().add("desktop");
		assertTrue(item.isApplicable("desktop"));
	}

	@Test
	void editItemIsNotApplicableWithNonMatchingUxUi() {
		EditItem item = new EditItem();
		item.getUxUis().add("mobile");
		assertFalse(item.isApplicable("desktop"));
	}

	@Test
	void editItemPropertiesNotNull() {
		EditItem item = new EditItem();
		assertThat(item.getProperties(), notNullValue());
	}

	// --- ListItem ---

	@Test
	void listItemAutoPopulateTrueByDefault() {
		ListItem item = new ListItem();
		assertTrue(item.isAutoPopulate());
	}

	@Test
	void listItemSetAutoPopulateToFalse() {
		ListItem item = new ListItem();
		item.setAutoPopulate(false);
		assertFalse(item.isAutoPopulate());
	}

	@Test
	void listItemSetAndGetName() {
		ListItem item = new ListItem();
		item.setName("listContacts");
		assertThat(item.getName(), is("listContacts"));
	}

	@Test
	void listItemIsApplicableWithEmptyUxUis() {
		ListItem item = new ListItem();
		assertTrue(item.isApplicable("desktop"));
	}

	@Test
	void menuItemDefaultGetLocalisedNameReturnsValue() {
		EditItem item = new EditItem();
		item.setName("Edit Contact");
		assertThat(item.getLocalisedName(), notNullValue());
	}
}
