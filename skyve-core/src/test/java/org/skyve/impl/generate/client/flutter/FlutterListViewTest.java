package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.FileWriter;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.view.model.list.ListModel;

@SuppressWarnings("boxing")
class FlutterListViewTest {
	@Test
	@SuppressWarnings("static-method")
	void addQuerySubstitutesUsesVisibleProjectedColumns() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		MetaDataQueryColumn hidden = mock(MetaDataQueryColumn.class);
		doReturn(true).when(hidden).isHidden();

		MetaDataQueryProjectedColumn notProjected = mock(MetaDataQueryProjectedColumn.class);
		doReturn(false).when(notProjected).isHidden();
		doReturn(false).when(notProjected).isProjected();

		MetaDataQueryColumn first = mock(MetaDataQueryColumn.class);
		doReturn(false).when(first).isHidden();
		when(first.getBinding()).thenReturn("displayName");

		MetaDataQueryColumn second = mock(MetaDataQueryColumn.class);
		doReturn(false).when(second).isHidden();
		when(second.getBinding()).thenReturn(null);
		when(second.getName()).thenReturn("code");

		when(query.getColumns()).thenReturn(List.of(hidden, notProjected, first, second));
		when(query.getLocalisedDescription()).thenReturn("Users");

		view.setQuery(mock(Module.class), document, query);
		Map<String, String> substitutions = new TreeMap<>();

		invokeAddQuerySubstitutes(view, substitutions);

		assertEquals("Users", substitutions.get("##DESCRIPTION##"));
		assertEquals("displayName", substitutions.get("##COLUMN1##"));
		assertEquals("code", substitutions.get("##COLUMN2##"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addQuerySubstitutesFallsBackToBizIdWhenNoColumns() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getColumns()).thenReturn(List.of());
		when(query.getLocalisedDescription()).thenReturn("Users");

		view.setQuery(mock(Module.class), document, query);
		Map<String, String> substitutions = new TreeMap<>();

		invokeAddQuerySubstitutes(view, substitutions);

		assertEquals(Bean.DOCUMENT_ID, substitutions.get("##COLUMN1##"));
		assertEquals(Bean.DOCUMENT_ID, substitutions.get("##COLUMN2##"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createWritesTemplateWithResolvedQueryValues() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		MetaDataQueryColumn column = mock(MetaDataQueryColumn.class);

		when(document.getName()).thenReturn("User");
		when(query.getName()).thenReturn("qUsers");
		when(query.getLocalisedDescription()).thenReturn("Users");
		when(query.getColumns()).thenReturn(List.of(column));
		doReturn(false).when(column).isHidden();
		when(column.getBinding()).thenReturn("displayName");

		view.setQuery(mock(Module.class), document, query);

		Path outputFile = Files.createTempFile("flutter-list-view", ".dart");
		try (FileWriter writer = new FileWriter(outputFile.toFile())) {
			view.create(writer);
		}

		String content = Files.readString(outputFile);
		assertTrue(content.contains("class AdminUserList extends StatefulWidget"));
		assertTrue(content.contains("fetchQuery('admin', null, 'qUsers'"));
		assertTrue(content.contains("'Users'"));
		assertTrue(content.contains("_rows[index]['displayName']"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addQuerySubstitutesUsesModelWhenQueryIsUnset() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		MetaDataQueryProjectedColumn projected = mock(MetaDataQueryProjectedColumn.class);

		doReturn(false).when(projected).isHidden();
		doReturn(true).when(projected).isProjected();
		when(projected.getBinding()).thenReturn("projectedName");
		when(projected.getName()).thenReturn("fallback");
		when(model.getColumns()).thenReturn(List.of(projected));
		when(model.getLocalisedDescription()).thenReturn("Model Users");

		view.setModel(mock(Module.class), document, "modelName", model);
		Map<String, String> substitutions = new TreeMap<>();

		invokeAddQuerySubstitutes(view, substitutions);

		assertEquals("Model Users", substitutions.get("##DESCRIPTION##"));
		assertEquals("projectedName", substitutions.get("##COLUMN1##"));
		assertEquals(Bean.DOCUMENT_ID, substitutions.get("##COLUMN2##"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setModelStoresDocumentAndModelReferences() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);

		view.setModel(mock(Module.class), document, "modelName", model);

		assertEquals(document, getField(view, "document"));
		assertEquals(model, getField(view, "model"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addQuerySubstitutesDefaultsWhenQueryAndModelAreUnset() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Map<String, String> substitutions = new TreeMap<>();

		invokeAddQuerySubstitutes(view, substitutions);

		assertEquals("", substitutions.get("##DESCRIPTION##"));
		assertEquals(Bean.DOCUMENT_ID, substitutions.get("##COLUMN1##"));
		assertEquals(Bean.DOCUMENT_ID, substitutions.get("##COLUMN2##"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addQuerySubstitutesStopsAfterFirstTwoVisibleColumns() throws Exception {
		FlutterListView view = new FlutterListView(mockGenerator("my_app"), "admin", "UserList");
		Document document = mock(Document.class);
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);

		MetaDataQueryColumn first = mock(MetaDataQueryColumn.class);
		doReturn(false).when(first).isHidden();
		when(first.getBinding()).thenReturn("firstVisible");

		MetaDataQueryColumn second = mock(MetaDataQueryColumn.class);
		doReturn(false).when(second).isHidden();
		when(second.getBinding()).thenReturn("secondVisible");

		MetaDataQueryColumn third = mock(MetaDataQueryColumn.class);
		doReturn(false).when(third).isHidden();
		when(third.getBinding()).thenReturn("thirdVisible");

		when(query.getColumns()).thenReturn(List.of(first, second, third));
		when(query.getLocalisedDescription()).thenReturn("Users");

		view.setQuery(mock(Module.class), document, query);
		Map<String, String> substitutions = new TreeMap<>();

		invokeAddQuerySubstitutes(view, substitutions);

		assertEquals("firstVisible", substitutions.get("##COLUMN1##"));
		assertEquals("secondVisible", substitutions.get("##COLUMN2##"));
	}

	private static FlutterGenerator mockGenerator(String projectName) {
		GeneratorConfig config = new GeneratorConfig();
		config.setProjectName(projectName);

		FlutterGenerator generator = mock(FlutterGenerator.class);
		when(generator.getConfig()).thenReturn(config);
		return generator;
	}

	private static void invokeAddQuerySubstitutes(FlutterListView view, Map<String, String> substitutions)
			throws Exception {
		Method method = FlutterListView.class.getDeclaredMethod("addQuerySubstitutes", Map.class);
		method.setAccessible(true);
		method.invoke(view, substitutions);
	}

	private static Object getField(Object instance, String fieldName) throws Exception {
		java.lang.reflect.Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return field.get(instance);
	}
}