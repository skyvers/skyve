package org.skyve.metadata.module.menu;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(menu.getItems().isEmpty(), is(true));
	}

	@Test
	void menuImplPropertiesNotNull() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.getProperties(), notNullValue());
	}

	@Test
	void menuImplIsApplicableReturnsFalseWhenNoItems() {
		MenuImpl menu = new MenuImpl();
		assertThat(menu.isApplicable("desktop"), is(false));
	}

	@Test
	void menuImplIsApplicableReturnsTrueWhenItemIsApplicable() {
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("testItem");
		// empty uxuis means applicable to all
		menu.getItems().add(item);
		assertThat(menu.isApplicable("desktop"), is(true));
	}

	@Test
	void menuImplIsApplicableReturnsFalseWhenItemNotApplicable() {
		MenuImpl menu = new MenuImpl();
		EditItem item = new EditItem();
		item.setName("testItem");
		item.getUxUis().add("mobile");
		menu.getItems().add(item);
		assertThat(menu.isApplicable("desktop"), is(false));
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
		assertThat(group.isApplicable("desktop"), is(true));
	}

	@Test
	void menuGroupImplToStringContainsName() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("MyGroup");
		assertThat(group.toString().contains("MyGroup"), is(true));
	}

	@Test
	void menuGroupImplIsApplicableReturnsFalseWhenUxUiNotMatched() {
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Mobile");
		group.getUxUis().add("mobile");
		assertThat(group.isApplicable("desktop"), is(false));
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
		assertThat(item.isApplicable("desktop"), is(true));
	}

	@Test
	void editItemIsApplicableWithMatchingUxUi() {
		EditItem item = new EditItem();
		item.getUxUis().add("desktop");
		assertThat(item.isApplicable("desktop"), is(true));
	}

	@Test
	void editItemIsNotApplicableWithNonMatchingUxUi() {
		EditItem item = new EditItem();
		item.getUxUis().add("mobile");
		assertThat(item.isApplicable("desktop"), is(false));
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
		assertThat(item.isAutoPopulate(), is(true));
	}

	@Test
	void listItemSetAutoPopulateToFalse() {
		ListItem item = new ListItem();
		item.setAutoPopulate(false);
		assertThat(item.isAutoPopulate(), is(false));
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
		assertThat(item.isApplicable("desktop"), is(true));
	}
}
