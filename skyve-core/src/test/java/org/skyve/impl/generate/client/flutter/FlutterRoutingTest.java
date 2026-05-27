package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiPredicate;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.menu.MenuItem;
import org.skyve.metadata.view.model.list.ListModel;
import org.mockito.ArgumentCaptor;

class FlutterRoutingTest {
	@Test
	@SuppressWarnings("static-method")
	void processItemAddsImportRouteMenuAndView() throws Exception {
		FlutterGenerator generator = mockGenerator("my_app");
		FlutterRouting routing = new FlutterRouting(generator);
		StubFlutterView view = new StubFlutterView(generator, "admin", "User");
		MenuItem item = mock(MenuItem.class);
		when(item.getLocalisedName()).thenReturn("Users");

		invokePrivate(routing, "processItem", new Class<?>[] {FlutterView.class, MenuItem.class}, view, item);

		Set<String> imports = privateSet(routing, "imports");
		Set<String> routes = privateSet(routing, "routes");
		String menu = privateMenu(routing);
		Set<FlutterView> views = generatorViews(generator);

		assertTrue(imports.contains("import 'package:my_app/views/admin/user.dart';"));
		assertTrue(routes.contains("GoRoute(path: AdminUser.routeName, builder: (context, state) => AdminUser(queryParams: state.queryParams)),"));
		assertTrue(menu.contains("title: 'Users'"));
		assertTrue(menu.contains("path: AdminUser.routeName"));
		assertTrue(views.contains(view));
	}

	@Test
	@SuppressWarnings("static-method")
	void processItemWithNullMenuItemSkipsMenuAppend() throws Exception {
		FlutterGenerator generator = mockGenerator("my_app");
		FlutterRouting routing = new FlutterRouting(generator);
		StubFlutterView view = new StubFlutterView(generator, "admin", "User");

		invokePrivate(routing, "processItem", new Class<?>[] {FlutterView.class, MenuItem.class}, view, null);

		String menu = privateMenu(routing);
		assertFalse(menu.contains("SkyveNavigationMenuItemModel("));
	}

	@Test
	@SuppressWarnings("static-method")
	void menuGroupAndModuleHelpersRestoreIndentationLevel() throws Exception {
		FlutterRouting routing = new FlutterRouting(mockGenerator("my_app"));

		invokePrivate(routing, "openMenuModule", new Class<?>[] {String.class, String.class}, "admin", "Admin");
		invokePrivate(routing, "openMenuGroup", new Class<?>[] {String.class}, "Group A");
		invokePrivate(routing, "closeMenuGroup", new Class<?>[0]);
		invokePrivate(routing, "closeMenuModule", new Class<?>[0]);

		int indentationLevel = privateInt(routing, "indentationLevel");
		String menu = privateMenu(routing);
		assertEquals(0, indentationLevel);
		assertTrue(menu.contains("SkyveModuleMenuModel("));
		assertTrue(menu.contains("SkyveMenuGroupModel(title: 'Group A', items: ["));
		assertTrue(menu.contains("]),\n"));
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void createWritesMainTemplateWithExpectedSubstitutions() throws Exception {
		FlutterGenerator generator = mockGenerator("my_app");
		FlutterRouting routing = new FlutterRouting(generator);

		StubFlutterView view = new StubFlutterView(generator, "admin", "User");
		MenuItem item = mock(MenuItem.class);
		when(item.getLocalisedName()).thenReturn("Users");
		invokePrivate(routing, "processItem", new Class<?>[] {FlutterView.class, MenuItem.class}, view, item);

		routing.create();

		ArgumentCaptor<Map<String, String>> substitutions = ArgumentCaptor.forClass(Map.class);
		verify(generator).refreshFile(org.mockito.ArgumentMatchers.eq("lib/main.dart"),
				org.mockito.ArgumentMatchers.eq("lib/main.dart"), substitutions.capture());

		Map<String, String> values = substitutions.getValue();
		assertTrue(values.get("##IMPORTS##").contains("import 'package:my_app/widgets/skyve_menu.dart';"));
		assertTrue(values.get("##IMPORTS##").contains("import 'package:my_app/views/admin/user.dart';"));
		assertTrue(values.get("##MENU##").contains("SkyveNavigationMenuItemModel("));
		assertTrue(values.get("##ROUTES##").contains("GoRoute(path: AdminUser.routeName"));
	}

	@Test
	@SuppressWarnings({"static-method", "unchecked"})
	void createAppliesWhitelistAndSkipsNonMatchingDocuments() throws Exception {
		GeneratorConfig config = new GeneratorConfig();
		config.setProjectName("my_app");
		config.setUxui("desktop");
		config.addModocWhitelistEntry("admin.User");

		Customer customer = mock(Customer.class);
		config.setCustomer(customer);

		Module adminModule = mock(Module.class);
		DocumentRef adminRef = mock(DocumentRef.class);
		Document adminDoc = mock(Document.class);
		Module crmModule = mock(Module.class);
		DocumentRef crmRef = mock(DocumentRef.class);
		Document crmDoc = mock(Document.class);

		Map<String, DocumentRef> adminRefs = new HashMap<>();
		adminRefs.put("User", adminRef);
		Map<String, DocumentRef> crmRefs = new HashMap<>();
		crmRefs.put("Contact", crmRef);

		when(adminModule.getName()).thenReturn("admin");
		when(adminModule.getMenu()).thenReturn(new MenuImpl());
		when(adminModule.getDocumentRefs()).thenReturn(adminRefs);
		when(adminRef.getOwningModuleName()).thenReturn("admin");
		when(adminModule.getDocument(customer, "User")).thenReturn(adminDoc);
		when(adminDoc.getName()).thenReturn("User");
		when(adminDoc.getView(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.eq(customer),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(null);

		when(crmModule.getName()).thenReturn("crm");
		when(crmModule.getMenu()).thenReturn(new MenuImpl());
		when(crmModule.getDocumentRefs()).thenReturn(crmRefs);
		when(crmRef.getOwningModuleName()).thenReturn("crm");
		when(crmModule.getDocument(customer, "Contact")).thenReturn(crmDoc);
		when(crmDoc.getName()).thenReturn("Contact");

		when(customer.getModules()).thenReturn(List.of(adminModule, crmModule));
		when(customer.getModule("admin")).thenReturn(adminModule);
		when(customer.getModule("crm")).thenReturn(crmModule);

		FlutterGenerator generator = mock(FlutterGenerator.class);
		when(generator.getConfig()).thenReturn(config);
		Field viewsField = FlutterGenerator.class.getDeclaredField("views");
		viewsField.setAccessible(true);
		viewsField.set(generator, new TreeSet<>());

		FlutterRouting routing = new FlutterRouting(generator);
		routing.create();

		ArgumentCaptor<Map<String, String>> substitutions = ArgumentCaptor.forClass(Map.class);
		verify(generator).refreshFile(org.mockito.ArgumentMatchers.eq("lib/main.dart"),
				org.mockito.ArgumentMatchers.eq("lib/main.dart"), substitutions.capture());

		String routes = substitutions.getValue().get("##ROUTES##");
		assertTrue(routes.contains("AdminUser.routeName"));
		assertFalse(routes.contains("CrmContact.routeName"));
	}

	@Test
	@SuppressWarnings("static-method")
	void viewImportsAndRoutesSkipsWhenWhitelistRejectsDocument() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Document document = mock(Document.class);

		when(module.getName()).thenReturn("admin");
		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("User", ref);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(ref.getOwningModuleName()).thenReturn("admin");
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(document.getName()).thenReturn("User");
		when(customer.getModules()).thenReturn(List.of(module));

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> denyAll = (m, d) -> false;
		invokePrivate(routing, "viewImportsAndRoutes", new Class<?>[] {BiPredicate.class}, denyAll);

		assertTrue(privateSet(routing, "imports").isEmpty());
		assertTrue(privateSet(routing, "routes").isEmpty());
		assertTrue(generatorViews(generator).isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void viewImportsAndRoutesAddsRouteWhenWhitelistAcceptsDocument() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Document document = mock(Document.class);

		when(module.getName()).thenReturn("admin");
		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("User", ref);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(ref.getOwningModuleName()).thenReturn("admin");
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(document.getName()).thenReturn("User");
		when(document.getView(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.eq(customer),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(null);
		when(customer.getModules()).thenReturn(List.of(module));

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "viewImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		assertTrue(privateSet(routing, "imports").contains("import 'package:my_app/views/admin/user.dart';"));
		assertTrue(privateSet(routing, "routes").contains(
				"GoRoute(path: AdminUser.routeName, builder: (context, state) => AdminUser(queryParams: state.queryParams)),"));
		assertEquals(1, generatorViews(generator).size());
	}

	@Test
	@SuppressWarnings("static-method")
	void viewImportsAndRoutesSkipsDocumentRefOwnedByDifferentModule() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);

		when(module.getName()).thenReturn("admin");
		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("User", ref);
		when(module.getDocumentRefs()).thenReturn(refs);
		when(ref.getOwningModuleName()).thenReturn("crm");
		when(customer.getModules()).thenReturn(List.of(module));

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "viewImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		assertTrue(privateSet(routing, "imports").isEmpty());
		assertTrue(privateSet(routing, "routes").isEmpty());
		assertTrue(generatorViews(generator).isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesSkipsEditRouteWhenWhitelistRejects() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		EditItem editItem = new EditItem();
		editItem.setName("Users");
		editItem.setDocumentName("User");
		menu.getItems().add(editItem);
		Document document = mock(Document.class);

		when(module.getName()).thenReturn("admin");
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");
		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> denyAll = (m, d) -> false;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, denyAll);

		assertTrue(privateSet(routing, "imports").isEmpty());
		assertTrue(privateSet(routing, "routes").isEmpty());
		assertTrue(generatorViews(generator).isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesAddsEditRouteAndGroupStructureWhenAllowed() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		MenuGroupImpl group = new MenuGroupImpl();
		group.setName("Group A");
		EditItem editItem = new EditItem();
		editItem.setName("Users");
		editItem.setDocumentName("User");
		group.getItems().add(editItem);
		menu.getItems().add(group);
		Document document = mock(Document.class);

		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getMenu()).thenReturn(menu);

		when(module.getDocument(customer, "User")).thenReturn(document);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");
		when(document.getName()).thenReturn("User");
		when(document.getView(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.eq(customer),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(null);

		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		assertTrue(privateSet(routing, "imports").contains("import 'package:my_app/views/admin/user.dart';"));
		assertTrue(privateSet(routing, "routes").contains(
				"GoRoute(path: AdminUser.routeName, builder: (context, state) => AdminUser(queryParams: state.queryParams)),"));
		assertEquals(1, generatorViews(generator).size());

		String menuText = privateMenu(routing);
		assertTrue(menuText.contains("SkyveModuleMenuModel("));
		assertTrue(menuText.contains("SkyveMenuGroupModel(title: 'Group A', items: ["));
		assertTrue(menuText.contains("title: 'Users'"));
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesAddsCalendarMapTreeAndModelListRoutes() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		Document document = mock(Document.class);

		CalendarItem calendar = new CalendarItem();
		calendar.setName("Calendar");
		calendar.setDocumentName("User");
		calendar.setQueryName("qCalendar");

		ListItem modelList = new ListItem();
		modelList.setName("ModelList");
		modelList.setDocumentName("User");
		modelList.setModelName("UserModel");

		MapItem map = new MapItem();
		map.setName("Map");
		map.setDocumentName("User");
		map.setModelName("UserModel");

		TreeItem tree = new TreeItem();
		tree.setName("Tree");
		tree.setDocumentName("User");
		tree.setModelName("UserModel");

		menu.getItems().add(calendar);
		menu.getItems().add(modelList);
		menu.getItems().add(map);
		menu.getItems().add(tree);

		MetaDataQueryDefinition calendarQuery = mock(MetaDataQueryDefinition.class);
		ListModel<org.skyve.domain.Bean> listModel = mock(ListModel.class);

		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(module.getMetaDataQuery("qCalendar")).thenReturn(calendarQuery);

		when(calendarQuery.getName()).thenReturn("qCalendar");
		when(calendarQuery.getDocumentName()).thenReturn("User");
		when(calendarQuery.getDocumentModule(customer)).thenReturn(module);

		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");
		when(document.getListModel(customer, "UserModel", false)).thenReturn(listModel);

		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		Set<String> routes = privateSet(routing, "routes");
		assertTrue(routes.stream().map(String::toLowerCase).anyMatch(r -> r.contains("qcalendarcal.routename")));
		assertTrue(routes.stream().anyMatch(r -> r.contains("AdminUserModelList.routeName")));
		assertTrue(routes.stream().anyMatch(r -> r.contains("AdminUserModelMap.routeName")));
		assertTrue(routes.stream().anyMatch(r -> r.contains("AdminUserModelTree.routeName")));
		assertEquals(4, generatorViews(generator).size());
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesCoversQueryAndDocumentDrivenListBranches() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		Document document = mock(Document.class);

		ListItem queryList = new ListItem();
		queryList.setName("QueryList");
		queryList.setDocumentName("User");
		queryList.setQueryName("qUsers");

		ListItem documentList = new ListItem();
		documentList.setName("DocumentList");
		documentList.setDocumentName("User");

		menu.getItems().add(queryList);
		menu.getItems().add(documentList);

		MetaDataQueryDefinition queryDriven = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition documentDriven = mock(MetaDataQueryDefinition.class);

		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(module.getMetaDataQuery("qUsers")).thenReturn(queryDriven);
		when(module.getNullSafeMetaDataQuery("qUsers")).thenReturn(queryDriven);
		when(module.getDocumentDefaultQuery(customer, "User")).thenReturn(documentDriven);
		when(queryDriven.getName()).thenReturn("qUsers");
		when(queryDriven.getDocumentName()).thenReturn("User");
		when(queryDriven.getDocumentModule(customer)).thenReturn(module);
		when(documentDriven.getName()).thenReturn("User");
		when(documentDriven.getDocumentName()).thenReturn("User");
		when(documentDriven.getDocumentModule(customer)).thenReturn(module);

		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("User");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");

		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		Set<String> routes = privateSet(routing, "routes");
		assertTrue(routes.stream().map(String::toLowerCase).anyMatch(r -> r.contains("quserslist.routename")));
		assertTrue(routes.stream().map(String::toLowerCase).anyMatch(r -> r.contains("userlist.routename")));
		assertEquals(2, generatorViews(generator).size());
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesSkipsCalendarListMapAndTreeWhenWhitelistRejects() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		Document document = mock(Document.class);

		CalendarItem calendar = new CalendarItem();
		calendar.setName("Calendar");
		calendar.setDocumentName("User");
		calendar.setQueryName("qCalendar");

		ListItem list = new ListItem();
		list.setName("List");
		list.setDocumentName("User");
		list.setQueryName("qUsers");

		MapItem map = new MapItem();
		map.setName("Map");
		map.setDocumentName("User");
		map.setQueryName("qMap");

		TreeItem tree = new TreeItem();
		tree.setName("Tree");
		tree.setDocumentName("User");
		tree.setQueryName("qTree");

		menu.getItems().add(calendar);
		menu.getItems().add(list);
		menu.getItems().add(map);
		menu.getItems().add(tree);

		MetaDataQueryDefinition calendarQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition listQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition mapQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition treeQuery = mock(MetaDataQueryDefinition.class);

		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(module.getMetaDataQuery("qCalendar")).thenReturn(calendarQuery);
		when(module.getMetaDataQuery("qUsers")).thenReturn(listQuery);
		when(module.getMetaDataQuery("qMap")).thenReturn(mapQuery);
		when(module.getMetaDataQuery("qTree")).thenReturn(treeQuery);
		when(module.getNullSafeMetaDataQuery("qUsers")).thenReturn(listQuery);

		when(calendarQuery.getName()).thenReturn("qCalendar");
		when(calendarQuery.getDocumentName()).thenReturn("User");
		when(calendarQuery.getDocumentModule(customer)).thenReturn(module);

		when(listQuery.getName()).thenReturn("qUsers");
		when(listQuery.getDocumentName()).thenReturn("User");
		when(listQuery.getDocumentModule(customer)).thenReturn(module);

		when(mapQuery.getName()).thenReturn("qMap");
		when(mapQuery.getDocumentName()).thenReturn("User");
		when(mapQuery.getDocumentModule(customer)).thenReturn(module);

		when(treeQuery.getName()).thenReturn("qTree");
		when(treeQuery.getDocumentName()).thenReturn("User");
		when(treeQuery.getDocumentModule(customer)).thenReturn(module);

		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("User");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");

		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> denyAll = (m, d) -> false;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, denyAll);

		assertTrue(privateSet(routing, "imports").isEmpty());
		assertTrue(privateSet(routing, "routes").isEmpty());
		assertTrue(generatorViews(generator).isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void menuImportsAndRoutesCoversRemainingCalendarMapTreeViewNameBranches() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		MenuImpl menu = new MenuImpl();
		Document document = mock(Document.class);

		CalendarItem calendar = new CalendarItem();
		calendar.setName("Calendar");
		calendar.setDocumentName("User");
		calendar.setQueryName("qCalendar");
		calendar.setModelName("CalendarModel");

		MapItem map = new MapItem();
		map.setName("Map");
		map.setDocumentName("User");
		map.setQueryName("qMap");

		TreeItem tree = new TreeItem();
		tree.setName("Tree");
		tree.setDocumentName("User");
		tree.setQueryName("qTree");

		menu.getItems().add(calendar);
		menu.getItems().add(map);
		menu.getItems().add(tree);

		MetaDataQueryDefinition calendarQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition mapQuery = mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition treeQuery = mock(MetaDataQueryDefinition.class);

		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getMenu()).thenReturn(menu);
		when(module.getDocument(customer, "User")).thenReturn(document);
		when(module.getMetaDataQuery("qCalendar")).thenReturn(calendarQuery);
		when(module.getMetaDataQuery("qMap")).thenReturn(mapQuery);
		when(module.getMetaDataQuery("qTree")).thenReturn(treeQuery);

		when(calendarQuery.getName()).thenReturn("qCalendar");
		when(calendarQuery.getDocumentName()).thenReturn("User");
		when(calendarQuery.getDocumentModule(customer)).thenReturn(module);

		when(mapQuery.getName()).thenReturn("qMap");
		when(mapQuery.getDocumentName()).thenReturn("User");
		when(mapQuery.getDocumentModule(customer)).thenReturn(module);

		when(treeQuery.getName()).thenReturn("qTree");
		when(treeQuery.getDocumentName()).thenReturn("User");
		when(treeQuery.getDocumentModule(customer)).thenReturn(module);

		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("User");
		when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		when(document.getIconStyleClass()).thenReturn("icon-user");

		when(customer.getModules()).thenReturn(List.of(module));
		when(customer.getModule("admin")).thenReturn(module);

		FlutterGenerator generator = mockGenerator("my_app", customer);
		FlutterRouting routing = new FlutterRouting(generator);

		BiPredicate<Module, Document> allowAll = (m, d) -> true;
		invokePrivate(routing, "menuImportsAndRoutes", new Class<?>[] {BiPredicate.class}, allowAll);

		Set<String> routes = privateSet(routing, "routes");
		assertTrue(routes.stream().anyMatch(r -> r.contains("AdminCalendarModelCal.routeName")));
		assertTrue(routes.stream().map(String::toLowerCase).anyMatch(r -> r.contains("qmapmap.routename")));
		assertTrue(routes.stream().map(String::toLowerCase).anyMatch(r -> r.contains("qtreetree.routename")));
		assertEquals(3, generatorViews(generator).size());
	}

	private static FlutterGenerator mockGenerator(String projectName) throws Exception {
		return mockGenerator(projectName, null);
	}

	private static FlutterGenerator mockGenerator(String projectName, Customer customer) throws Exception {
		GeneratorConfig config = new GeneratorConfig();
		config.setProjectName(projectName);
		config.setUxui("desktop");

		Customer effectiveCustomer = customer;
		if (effectiveCustomer == null) {
			effectiveCustomer = mock(Customer.class);
			when(effectiveCustomer.getModules()).thenReturn(Collections.emptyList());
		}
		config.setCustomer(effectiveCustomer);

		FlutterGenerator generator = mock(FlutterGenerator.class);
		when(generator.getConfig()).thenReturn(config);

		Field viewsField = FlutterGenerator.class.getDeclaredField("views");
		viewsField.setAccessible(true);
		viewsField.set(generator, new TreeSet<>());
		return generator;
	}

	@SuppressWarnings("unchecked")
	private static Set<String> privateSet(Object instance, String fieldName) throws Exception {
		Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return (Set<String>) field.get(instance);
	}

	@SuppressWarnings("unchecked")
	private static Set<FlutterView> generatorViews(FlutterGenerator generator) throws Exception {
		Field field = FlutterGenerator.class.getDeclaredField("views");
		field.setAccessible(true);
		return (Set<FlutterView>) field.get(generator);
	}

	private static String privateMenu(FlutterRouting routing) throws Exception {
		Field field = FlutterRouting.class.getDeclaredField("menu");
		field.setAccessible(true);
		return field.get(routing).toString();
	}

	private static int privateInt(FlutterRouting routing, String fieldName) throws Exception {
		Field field = FlutterRouting.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		return field.getInt(routing);
	}

	private static Object invokePrivate(Object instance, String methodName, Class<?>[] types, Object... args)
			throws Exception {
		Method method = instance.getClass().getDeclaredMethod(methodName, types);
		method.setAccessible(true);
		return method.invoke(instance, args);
	}

	private static final class StubFlutterView extends FlutterView {
		private StubFlutterView(FlutterGenerator generator, String moduleName, String viewName) {
			super(generator, moduleName, viewName);
		}

		@Override
		protected void create(FileWriter fw) throws IOException {
			// no-op for routing tests
		}
	}
}