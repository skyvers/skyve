package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.MenuImpl;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;

class ReactNativeRouterTest {
	@Test
	@SuppressWarnings("static-method")
	void processItemAddsImportRouteAndComponent() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-test");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);
		ReactComponent component = new StubReactComponent(generator, "admin", "User");

		invokeProcessItem(router, component, null);

		Set<String> imports = privateSet(router, "imports");
		Set<String> routes = privateSet(router, "routes");

		assertTrue(imports.contains("import adminUser from './views/admin/User';\n"));
		assertTrue(routes.contains("\t\t\t<Stack.Screen name=\"adminUser\" component={adminUser} />\n"));
		assertTrue(generator.components.contains(component));
	}

	@Test
	@SuppressWarnings("static-method")
	void processItemDeduplicatesImportsAndRoutes() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-dedupe");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);
		ReactComponent component = new StubReactComponent(generator, "admin", "User");

		invokeProcessItem(router, component, null);
		invokeProcessItem(router, component, new String[] {"ignored"});

		Set<String> imports = privateSet(router, "imports");
		Set<String> routes = privateSet(router, "routes");
		assertEquals(1, imports.size());
		assertEquals(1, routes.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void createWritesNativeRouterFileWithBaseContent() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-create");
		Files.createDirectories(projectDir.resolve("src/skyve"));
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);

		Customer customer = org.mockito.Mockito.mock(Customer.class);
		org.mockito.Mockito.when(customer.getModules()).thenReturn(java.util.Collections.emptyList());
		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = org.mockito.Mockito.mock(AbstractPersistence.class);
		org.mockito.Mockito.when(persistence.getUser()).thenReturn(user);

		AtomicReference<Object> previous = setThreadLocalPersistence(persistence);
		try {
			router.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/Router.js"));
		assertTrue(content.contains("import { createStackNavigator } from '@react-navigation/stack';"));
		assertTrue(content.contains("export default function Router()"));
		assertTrue(content.contains("<Stack.Screen name=\"Home\" component={Home}"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createExecutesMenuCallbacksAndViewRouting() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-menu-create");
		Files.createDirectories(projectDir.resolve("src/skyve"));
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);

		Customer customer = org.mockito.Mockito.mock(Customer.class);
		Module module = org.mockito.Mockito.mock(Module.class);
		Document document = org.mockito.Mockito.mock(Document.class);
		DocumentRef ref = org.mockito.Mockito.mock(DocumentRef.class);
		MenuImpl menu = new MenuImpl();

		EditItem editItem = new EditItem();
		editItem.setName("Edit");
		editItem.setDocumentName("User");

		ListItem modelList = new ListItem();
		modelList.setName("ModelList");
		modelList.setDocumentName("User");
		modelList.setModelName("UserModel");

		ListItem queryList = new ListItem();
		queryList.setName("QueryList");
		queryList.setDocumentName("User");
		queryList.setQueryName("qUsers");

		ListItem defaultList = new ListItem();
		defaultList.setName("DefaultList");
		defaultList.setDocumentName("User");

		MapItem mapItem = new MapItem();
		mapItem.setName("Map");
		mapItem.setDocumentName("User");
		mapItem.setModelName("UserModel");

		TreeItem treeItem = new TreeItem();
		treeItem.setName("Tree");
		treeItem.setDocumentName("User");
		treeItem.setModelName("UserModel");

		CalendarItem calendarItem = new CalendarItem();
		calendarItem.setName("Calendar");
		calendarItem.setDocumentName("User");
		calendarItem.setQueryName("qCalendar");

		menu.getItems().add(editItem);
		menu.getItems().add(modelList);
		menu.getItems().add(queryList);
		menu.getItems().add(defaultList);
		menu.getItems().add(mapItem);
		menu.getItems().add(treeItem);
		menu.getItems().add(calendarItem);

		MetaDataQueryDefinition qCalendar = org.mockito.Mockito.mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition qUsers = org.mockito.Mockito.mock(MetaDataQueryDefinition.class);
		MetaDataQueryDefinition defaultQuery = org.mockito.Mockito.mock(MetaDataQueryDefinition.class);
		ListModel<org.skyve.domain.Bean> listModel = org.mockito.Mockito.mock(ListModel.class);

		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		org.mockito.Mockito.when(module.getMenu()).thenReturn(menu);
		org.mockito.Mockito.when(module.getDocument(customer, "User")).thenReturn(document);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("User", ref);
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(refs);
		org.mockito.Mockito.when(ref.getOwningModuleName()).thenReturn("admin");

		org.mockito.Mockito.when(module.getMetaDataQuery("qCalendar")).thenReturn(qCalendar);
		org.mockito.Mockito.when(module.getMetaDataQuery("qUsers")).thenReturn(qUsers);
		org.mockito.Mockito.when(module.getNullSafeMetaDataQuery("qUsers")).thenReturn(qUsers);
		org.mockito.Mockito.when(module.getDocumentDefaultQuery(customer, "User")).thenReturn(defaultQuery);

		org.mockito.Mockito.when(qCalendar.getName()).thenReturn("qCalendar");
		org.mockito.Mockito.when(qCalendar.getDocumentName()).thenReturn("User");
		org.mockito.Mockito.when(qCalendar.getDocumentModule(customer)).thenReturn(module);
		org.mockito.Mockito.when(qUsers.getName()).thenReturn("qUsers");
		org.mockito.Mockito.when(qUsers.getDocumentName()).thenReturn("User");
		org.mockito.Mockito.when(qUsers.getDocumentModule(customer)).thenReturn(module);
		org.mockito.Mockito.when(defaultQuery.getName()).thenReturn("User");
		org.mockito.Mockito.when(defaultQuery.getDocumentName()).thenReturn("User");
		org.mockito.Mockito.when(defaultQuery.getDocumentModule(customer)).thenReturn(module);

		org.mockito.Mockito.when(document.getName()).thenReturn("User");
		org.mockito.Mockito.when(document.getOwningModuleName()).thenReturn("admin");
		org.mockito.Mockito.when(document.getIcon16x16RelativeFileName()).thenReturn("user.png");
		org.mockito.Mockito.when(document.getIconStyleClass()).thenReturn("icon-user");
		org.mockito.Mockito.when(document.getListModel(customer, "UserModel", false)).thenReturn(listModel);
		org.mockito.Mockito.when(document.getView(org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.eq(customer),
				org.mockito.ArgumentMatchers.anyString())).thenReturn(null);

		org.mockito.Mockito.when(customer.getModules()).thenReturn(List.of(module));
		org.mockito.Mockito.when(customer.getModule("admin")).thenReturn(module);

		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = org.mockito.Mockito.mock(AbstractPersistence.class);
		org.mockito.Mockito.when(persistence.getUser()).thenReturn(user);

		AtomicReference<Object> previous = setThreadLocalPersistence(persistence);
		try {
			router.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/Router.js"));
		assertTrue(content.contains("import adminUser from './views/admin/User';"));
		assertTrue(content.contains("<Stack.Screen name=\"adminUser\" component={adminUser} />"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createReplacesExistingRouterFile() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-replace");
		Path routerPath = projectDir.resolve("src/skyve/Router.js");
		Files.createDirectories(routerPath.getParent());
		Files.writeString(routerPath, "OLD_CONTENT");

		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);

		Customer customer = org.mockito.Mockito.mock(Customer.class);
		org.mockito.Mockito.when(customer.getModules()).thenReturn(java.util.Collections.emptyList());
		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = org.mockito.Mockito.mock(AbstractPersistence.class);
		org.mockito.Mockito.when(persistence.getUser()).thenReturn(user);

		AtomicReference<Object> previous = setThreadLocalPersistence(persistence);
		try {
			router.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(routerPath);
		assertFalse(content.contains("OLD_CONTENT"));
		assertTrue(content.contains("export default function Router()"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createSkipsViewImportWhenDocumentRefOwnedByDifferentModule() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-router-foreign-ref");
		Files.createDirectories(projectDir.resolve("src/skyve"));
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactNativeRouter router = new ReactNativeRouter(generator);

		Customer customer = org.mockito.Mockito.mock(Customer.class);
		Module module = org.mockito.Mockito.mock(Module.class);
		DocumentRef ref = org.mockito.Mockito.mock(DocumentRef.class);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("User", ref);

		org.mockito.Mockito.when(module.getName()).thenReturn("admin");
		org.mockito.Mockito.when(module.getMenu()).thenReturn(new MenuImpl());
		org.mockito.Mockito.when(module.getDocumentRefs()).thenReturn(refs);
		org.mockito.Mockito.when(ref.getOwningModuleName()).thenReturn("crm");

		org.mockito.Mockito.when(customer.getModules()).thenReturn(List.of(module));
		org.mockito.Mockito.when(customer.getModule("admin")).thenReturn(module);

		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = org.mockito.Mockito.mock(AbstractPersistence.class);
		org.mockito.Mockito.when(persistence.getUser()).thenReturn(user);

		AtomicReference<Object> previous = setThreadLocalPersistence(persistence);
		try {
			router.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/Router.js"));
		assertFalse(content.contains("import adminUser from './views/admin/User';"));
	}

	@SuppressWarnings("unchecked")
	private static Set<String> privateSet(Object instance, String fieldName) throws Exception {
		Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return (Set<String>) field.get(instance);
	}

	private static void invokeProcessItem(ReactNativeRouter router, ReactComponent component, String[] params)
			throws Exception {
		Method method = ReactNativeRouter.class.getDeclaredMethod("processItem", ReactComponent.class, String[].class);
		method.setAccessible(true);
		method.invoke(router, component, params);
	}

	@SuppressWarnings("unchecked")
	private static AtomicReference<Object> setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<?> threadLocal = (ThreadLocal<?>) threadLocalField.get(null);
		AtomicReference<Object> previous = new AtomicReference<>(threadLocal.get());
		((ThreadLocal<Object>) threadLocal).set(persistence);
		return previous;
	}

	@SuppressWarnings("unchecked")
	private static void restoreThreadLocalPersistence(Object previous) throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<?> threadLocal = (ThreadLocal<?>) threadLocalField.get(null);
		if (previous == null) {
			threadLocal.remove();
		} else {
			((ThreadLocal<Object>) threadLocal).set(previous);
		}
	}

	private static final class StubReactComponent extends ReactComponent {
		private StubReactComponent(ReactGenerator generator, String moduleName, String componentName) {
			super(generator, moduleName, componentName);
		}

		@Override
		protected void create(FileWriter fw) throws IOException {
			// no-op for routing tests
		}
	}
}