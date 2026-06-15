package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;

@SuppressWarnings("boxing")
class ReactListViewTest {
	@Test
	@SuppressWarnings("static-method")
	void createWithModelRendersFetchModelAndVisibleColumns() throws Exception {
		Path projectDir = Files.createTempDirectory("react-list-model");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactListView view = new ReactListView(generator, "admin", "UserList");

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		Document drivingDocument = mock(Document.class);
		MetaDataQueryColumn hidden = mock(MetaDataQueryColumn.class);
		MetaDataQueryProjectedColumn notProjected = mock(MetaDataQueryProjectedColumn.class);
		MetaDataQueryColumn firstVisible = mock(MetaDataQueryColumn.class);
		MetaDataQueryColumn secondVisible = mock(MetaDataQueryColumn.class);

		when(drivingDocument.getOwningModuleName()).thenReturn("admin");
		when(model.getDrivingDocument()).thenReturn(drivingDocument);
		when(model.getColumns()).thenReturn(List.of(hidden, notProjected, firstVisible, secondVisible));

		when(hidden.isHidden()).thenReturn(true);

		when(notProjected.isHidden()).thenReturn(false);
		when(notProjected.isProjected()).thenReturn(false);

		when(firstVisible.isHidden()).thenReturn(false);
		when(firstVisible.getBinding()).thenReturn("displayName");
		when(firstVisible.getDisplayName()).thenReturn("Display Name");

		when(secondVisible.isHidden()).thenReturn(false);
		when(secondVisible.getBinding()).thenReturn(null);
		when(secondVisible.getName()).thenReturn("code");
		when(secondVisible.getDisplayName()).thenReturn("Code");

		view.setModel(module, document, "UserModel", model);

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence());
		try {
			view.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/UserList.js"));
		assertTrue(content.contains("this.fetchModel('admin', 'UserModel', 0, 75)"));
		assertTrue(content.contains("<Column field=\"displayName\" header=\"Display Name\" />"));
		assertTrue(content.contains("<Column field=\"code\" header=\"Code\" />"));
		assertFalse(content.contains("notProjected"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createIncludesProjectedColumnWhenProjectedFlagIsTrue() throws Exception {
		Path projectDir = Files.createTempDirectory("react-list-projected");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactListView view = new ReactListView(generator, "admin", "ProjectedList");

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Module owningModule = mock(Module.class);
		MetaDataQueryProjectedColumn projected = mock(MetaDataQueryProjectedColumn.class);

		when(query.getOwningModule()).thenReturn(owningModule);
		when(owningModule.getName()).thenReturn("admin");
		when(query.getName()).thenReturn("qProjected");
		when(query.getColumns()).thenReturn(List.of(projected));

		when(projected.isHidden()).thenReturn(false);
		when(projected.isProjected()).thenReturn(true);
		when(projected.getBinding()).thenReturn("projectedName");
		when(projected.getDisplayName()).thenReturn("Projected Name");

		view.setQuery(module, document, query);

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence());
		try {
			view.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/ProjectedList.js"));
		assertTrue(content.contains("<Column field=\"projectedName\" header=\"Projected Name\" />"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createWithQueryRendersFetchQueryBranch() throws Exception {
		Path projectDir = Files.createTempDirectory("react-list-query");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactListView view = new ReactListView(generator, "admin", "UserQueryList");

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		Module owningModule = mock(Module.class);

		when(query.getOwningModule()).thenReturn(owningModule);
		when(owningModule.getName()).thenReturn("crm");
		when(query.getName()).thenReturn("qUsers");
		when(query.getColumns()).thenReturn(List.of());

		view.setQuery(module, document, query);

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence());
		try {
			view.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/UserQueryList.js"));
		assertTrue(content.contains("this.fetchQuery('crm', 'qUsers', 0, 75)"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createWithoutModelOrQueryLeavesComponentDidMountEmpty() throws Exception {
		Path projectDir = Files.createTempDirectory("react-list-empty");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactListView view = new ReactListView(generator, "admin", "EmptyList");
		view.create();

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/EmptyList.js"));
		assertTrue(content.contains("componentDidMount() {"));
		assertFalse(content.contains("this.fetchModel("));
		assertFalse(content.contains("this.fetchQuery("));
	}

	private static AbstractPersistence mockPersistence() {
		Customer customer = mock(Customer.class);
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		return persistence;
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
}