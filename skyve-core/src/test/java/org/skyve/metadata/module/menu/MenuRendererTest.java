package org.skyve.metadata.module.menu;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;

@SuppressWarnings("static-method")
class MenuRendererTest {

	private static class RecordingMenuRenderer extends MenuRenderer {
		private final List<Boolean> openFlags = new ArrayList<>();
		private int moduleMenuCalls;
		private int menuGroupCalls;
		private int linkItemCalls;
		private int listItemCalls;
		private int treeItemCalls;
		private int calendarItemCalls;
		private int mapItemCalls;
		private int editItemCalls;
		private boolean lastRelative;
		private String lastHref;

		RecordingMenuRenderer(String uxui, String selectedModuleName) {
			super(uxui, selectedModuleName);
		}

		@Override
		public void renderModuleMenu(Menu menu, Module menuModule, boolean open) {
			moduleMenuCalls++;
			openFlags.add(Boolean.valueOf(open));
		}

		@Override
		public void renderMenuGroup(MenuGroup group, Module menuModule) {
			menuGroupCalls++;
		}

		@Override
		public void renderLinkItem(LinkItem item, Module menuModule, boolean relative, String absoluteHref) {
			linkItemCalls++;
			lastRelative = relative;
			lastHref = absoluteHref;
		}

		@Override
		public void renderListItem(org.skyve.impl.metadata.module.menu.ListItem item,
									Module menuModule,
									Module itemModule,
									org.skyve.metadata.model.document.Document itemDocument,
									String itemQueryName,
									String icon16,
									String iconStyleClass) {
			listItemCalls++;
		}

		@Override
		public void renderTreeItem(TreeItem item,
									Module menuModule,
									Module itemModule,
									Document itemDocument,
									String itemQueryName,
									String icon16,
									String iconStyleClass) {
			treeItemCalls++;
		}

		@Override
		public void renderCalendarItem(CalendarItem item,
									Module menuModule,
									Module itemModule,
									Document itemDocument,
									String itemQueryName,
									String icon16,
									String iconStyleClass) {
			calendarItemCalls++;
		}

		@Override
		public void renderMapItem(MapItem item,
									Module menuModule,
									Module itemModule,
									Document itemDocument,
									String itemQueryName,
									String icon16,
									String iconStyleClass) {
			mapItemCalls++;
		}

		@Override
		public void renderEditItem(EditItem item,
									Module menuModule,
									Module itemModule,
									Document itemDocument,
									String icon16,
									String iconStyleClass) {
			editItemCalls++;
		}
	}

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new RecordingMenuRenderer("desktop", null));
	}

	@Test
	void renderModuleMenuDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderModuleMenu(null, null, false));
	}

	@Test
	void renderMenuRootDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderMenuRoot(null, null));
	}

	@Test
	void renderMenuGroupDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderMenuGroup(null, null));
	}

	@Test
	void renderTreeItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderTreeItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderListItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderListItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderCalendarItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderCalendarItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderMapItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderMapItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderEditItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderEditItem(null, null, null, null, null, null));
	}

	@Test
	void renderLinkItemDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderLinkItem(null, null, false, null));
	}

	@Test
	void renderedMenuGroupDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderedMenuGroup(null, null));
	}

	@Test
	void renderedMenuRootDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderedMenuRoot(null, null));
	}

	@Test
	void renderedModuleMenuDoesNotThrow() {
		assertDoesNotThrow(() -> new RecordingMenuRenderer("desktop", null).renderedModuleMenu(null, null, false));
	}

	@Test
	void deriveDocumentQueryReturnsNamedQueryWhenValid() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuItem item = mock(MenuItem.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		when(item.getName()).thenReturn("menuItem");
		when(module.getName()).thenReturn("admin");
		when(module.getMetaDataQuery("q1")).thenReturn(query);
		when(query.getName()).thenReturn("q1");

		MetaDataQueryDefinition result = MenuRenderer.deriveDocumentQuery(customer, module, item, "q1", "Contact");
		assertThat(result, is(query));
	}

	@Test
	void deriveDocumentQueryThrowsWhenNamedQueryIsInvalid() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuItem item = mock(MenuItem.class);

		when(item.getName()).thenReturn("menuItem");
		when(module.getName()).thenReturn("admin");
		when(module.getMetaDataQuery("missing")).thenReturn(null);

		MetaDataException ex = assertThrows(MetaDataException.class,
				() -> MenuRenderer.deriveDocumentQuery(customer, module, item, "missing", "Contact"));
		assertThat(ex.getMessage(), containsString("target query missing"));
	}

	@Test
	void deriveDocumentQueryFallsBackToDefaultQueryWhenQueryNameNull() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuItem item = mock(MenuItem.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		when(module.getDocumentDefaultQuery(customer, "Contact")).thenReturn(query);

		MetaDataQueryDefinition result = MenuRenderer.deriveDocumentQuery(customer, module, item, null, "Contact");
		assertThat(result, is(query));
	}

	@Test
	void deriveDocumentQueryWrapsDefaultQueryFailure() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuItem item = mock(MenuItem.class);

		when(item.getName()).thenReturn("menuItem");
		when(module.getName()).thenReturn("admin");
		when(module.getDocumentDefaultQuery(customer, "Contact")).thenThrow(new MetaDataException("boom"));

		MetaDataException ex = assertThrows(MetaDataException.class,
				() -> MenuRenderer.deriveDocumentQuery(customer, module, item, null, "Contact"));
		assertThat(ex.getMessage(), containsString("target document Contact"));
	}

	@Test
	void renderCustomerOpensFirstApplicableModuleWhenNoSelection() {
		Customer customer = mock(Customer.class);
		Module moduleA = mock(Module.class);
		Module moduleB = mock(Module.class);

		MenuImpl menuA = new MenuImpl();
		LinkItem link = new LinkItem();
		link.setName("link");
		link.setHref("relative/page");
		menuA.getItems().add(link);
		MenuImpl menuB = new MenuImpl();

		when(customer.getModules()).thenReturn(List.of(moduleA, moduleB));
		when(moduleA.getName()).thenReturn("admin");
		when(moduleB.getName()).thenReturn("crm");
		when(customer.getModule("admin")).thenReturn(moduleA);
		when(customer.getModule("crm")).thenReturn(moduleB);
		when(moduleA.getMenu()).thenReturn(menuA);
		when(moduleB.getMenu()).thenReturn(menuB);

		RecordingMenuRenderer renderer = new RecordingMenuRenderer(null, null);
		renderer.render(customer);

		assertEquals(2, renderer.moduleMenuCalls);
		assertThat(renderer.openFlags.get(0), is(Boolean.TRUE));
		assertThat(renderer.openFlags.get(1), is(Boolean.FALSE));
		assertEquals(1, renderer.linkItemCalls);
		assertTrue(renderer.lastRelative);
		assertThat(renderer.lastHref, containsString("relative/page"));
	}

	@Test
	void renderCustomerSupportsGroupAndSelectedModuleOpenState() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		MenuImpl menu = new MenuImpl();
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("group");
		LinkItem absolute = new LinkItem();
		absolute.setName("abs");
		absolute.setHref("https://example.com");
		group.getItems().add(absolute);
		menu.getItems().add(group);

		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMenu()).thenReturn(menu);

		RecordingMenuRenderer renderer = new RecordingMenuRenderer("desktop", "admin");
		renderer.render(customer);

		assertEquals(1, renderer.moduleMenuCalls);
		assertThat(renderer.openFlags.get(0), is(Boolean.TRUE));
		assertEquals(1, renderer.menuGroupCalls);
		assertEquals(1, renderer.linkItemCalls);
		assertFalse(renderer.lastRelative);
		assertThat(renderer.lastHref, is("https://example.com"));
	}

	@Test
	void renderUserUsesUserSpecificMenusAndRendersListItemModelPath() {
		UserImpl user = mock(UserImpl.class);
		Customer customer = mock(Customer.class);
		Module menuModule = mock(Module.class);
		Module itemModule = mock(Module.class);
		org.skyve.metadata.model.document.Document menuDocument = mock(org.skyve.metadata.model.document.Document.class);
		org.skyve.metadata.model.document.Document itemDocument = mock(org.skyve.metadata.model.document.Document.class);

		MenuImpl menu = new MenuImpl();
		ListItem list = new ListItem();
		list.setName("list");
		list.setDocumentName("Contact");
		list.setModelName("myModel");
		menu.getItems().add(list);

		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModules()).thenReturn(List.of(menuModule));
		when(menuModule.getName()).thenReturn("admin");
		when(user.getModuleMenu("admin")).thenReturn(menu);
		when(menuModule.getDocument(customer, "Contact")).thenReturn(menuDocument);
		when(menuDocument.getOwningModuleName()).thenReturn("crm");
		when(customer.getModule("crm")).thenReturn(itemModule);
		when(itemModule.getDocument(customer, "Contact")).thenReturn(itemDocument);
		when(itemDocument.getIcon16x16RelativeFileName()).thenReturn("icon.png");
		when(itemDocument.getIconStyleClass()).thenReturn("icon-style");

		RecordingMenuRenderer renderer = new RecordingMenuRenderer("desktop", "admin");
		renderer.render(user);

		assertEquals(1, renderer.listItemCalls);
	}

	@Test
	void renderCustomerSkipsModuleWhenMenuNotApplicableForUxUi() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		MenuImpl menu = new MenuImpl();
		LinkItem mobileOnly = new LinkItem();
		mobileOnly.setName("mobile");
		mobileOnly.setHref("mobile");
		mobileOnly.getUxUis().add("mobile");
		menu.getItems().add(mobileOnly);

		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMenu()).thenReturn(menu);

		RecordingMenuRenderer renderer = new RecordingMenuRenderer("desktop", null);
		renderer.render(customer);

		assertEquals(0, renderer.moduleMenuCalls);
	}

	@Test
	void renderCustomerOpensOnlySelectedModuleWhenSelectionExists() {
		Customer customer = mock(Customer.class);
		Module moduleA = mock(Module.class);
		Module moduleB = mock(Module.class);

		MenuImpl menuA = new MenuImpl();
		LinkItem linkA = new LinkItem();
		linkA.setName("a");
		linkA.setHref("a");
		menuA.getItems().add(linkA);

		MenuImpl menuB = new MenuImpl();
		LinkItem linkB = new LinkItem();
		linkB.setName("b");
		linkB.setHref("b");
		menuB.getItems().add(linkB);

		when(customer.getModules()).thenReturn(List.of(moduleA, moduleB));
		when(moduleA.getName()).thenReturn("admin");
		when(moduleB.getName()).thenReturn("crm");
		when(customer.getModule("admin")).thenReturn(moduleA);
		when(customer.getModule("crm")).thenReturn(moduleB);
		when(moduleA.getMenu()).thenReturn(menuA);
		when(moduleB.getMenu()).thenReturn(menuB);

		RecordingMenuRenderer renderer = new RecordingMenuRenderer(null, "crm");
		renderer.render(customer);

		assertEquals(2, renderer.moduleMenuCalls);
		assertThat(renderer.openFlags.get(0), is(Boolean.FALSE));
		assertThat(renderer.openFlags.get(1), is(Boolean.TRUE));
	}

	@Test
	void renderCustomerCoversTreeMapCalendarAndEditBranches() {
		Customer customer = mock(Customer.class);
		Module menuModule = mock(Module.class);
		Module queryModule = mock(Module.class);

		Document menuDoc = mock(Document.class);
		Document targetDoc = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		MenuImpl menu = new MenuImpl();

		TreeItem tree = new TreeItem();
		tree.setName("tree");
		tree.setDocumentName("Contact");
		tree.setQueryName("q1");
		menu.getItems().add(tree);

		MapItem map = new MapItem();
		map.setName("map");
		map.setDocumentName("Contact");
		map.setQueryName("q1");
		menu.getItems().add(map);

		CalendarItem calendar = new CalendarItem();
		calendar.setName("calendar");
		calendar.setDocumentName("Contact");
		calendar.setQueryName("q1");
		menu.getItems().add(calendar);

		EditItem edit = new EditItem();
		edit.setName("edit");
		edit.setDocumentName("Contact");
		menu.getItems().add(edit);

		when(customer.getModules()).thenReturn(List.of(menuModule));
		when(menuModule.getName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(menuModule);
		when(menuModule.getMenu()).thenReturn(menu);

		when(menuModule.getMetaDataQuery("q1")).thenReturn(query);
		when(query.getName()).thenReturn("q1");
		when(query.getDocumentName()).thenReturn("Contact");
		when(query.getDocumentModule(customer)).thenReturn(queryModule);

		when(menuModule.getDocument(customer, "Contact")).thenReturn(menuDoc);
		when(queryModule.getDocument(customer, "Contact")).thenReturn(targetDoc);
		when(menuDoc.getOwningModuleName()).thenReturn("admin");
		when(targetDoc.getOwningModuleName()).thenReturn("admin");
		when(targetDoc.getIcon16x16RelativeFileName()).thenReturn("icon.png");
		when(targetDoc.getIconStyleClass()).thenReturn("icon-style");
		when(menuDoc.getIcon16x16RelativeFileName()).thenReturn("edit.png");
		when(menuDoc.getIconStyleClass()).thenReturn("edit-style");

		RecordingMenuRenderer renderer = new RecordingMenuRenderer("desktop", "admin");
		renderer.render(customer);

		assertEquals(1, renderer.treeItemCalls);
		assertEquals(1, renderer.mapItemCalls);
		assertEquals(1, renderer.calendarItemCalls);
		assertEquals(1, renderer.editItemCalls);
	}

	@Test
	void renderCustomerBuildsContextHrefForRootRelativeLink() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		MenuImpl menu = new MenuImpl();
		LinkItem link = new LinkItem();
		link.setName("rootLink");
		link.setHref("/health");
		menu.getItems().add(link);

		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getMenu()).thenReturn(menu);

		RecordingMenuRenderer renderer = new RecordingMenuRenderer(null, null);
		renderer.render(customer);

		assertTrue(renderer.lastRelative);
		assertThat(renderer.lastHref, containsString("/health"));
	}
}
