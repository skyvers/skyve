package org.skyve.impl.sail.recording;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.component.column.Column;
import org.primefaces.component.datatable.DataTable;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.JSON;
import org.skyve.web.UserAgentType;

import modules.test.AbstractSkyveTest;
import util.sail.SailUsers;

/**
 * Tests the PrimeFaces recorder's client-id map against kitchen sink and admin views: each element a step acts on is
 * named as the step names it, under the client id the page gives it, and the map follows the user's real roles.
 */
@SuppressWarnings({ "unchecked", "java:S5960" }) // JSON maps; test-only assertions
class PrimeFacesRecordingMapH2Test extends AbstractSkyveTest {
	@AfterEach
	void restoreUser() {
		AbstractPersistence.get().setUser(u);
	}

	/**
	 * The Actions Fixture's edit view, as a user with every role (as admin signs in to the SAIL server). The client ids
	 * checked are the ones the server renders for this view, which the PrimeFaces kitchen sink script clicks.
	 */
	@Test
	void actionsFixtureNamesEachElementAsItsStepDoes() throws Exception {
		AbstractPersistence.get().setUser(SailUsers.bootstrapLikeUser(u));
		Map<String, Object> map = parse(PrimeFacesRecordingMap.edit("kitchensink", "ActionsFixture", "tablet", UserAgentType.tablet));
		Map<String, Map<String, Object>> ids = (Map<String, Map<String, Object>>) map.get("ids");
		Collection<Map<String, Object>> entries = ids.values();
		Map<String, Object> rows = (Map<String, Object>) ((Map<String, Object>) map.get("grids")).get("rows");
		List<Map<String, Object>> columns = (List<Map<String, Object>>) rows.get("columns");

		assertAll(() -> assertEquals("kitchensink", map.get("module")),
					() -> assertEquals("ActionsFixture", map.get("document")),
					() -> assertEquals("tablet", map.get("uxui")),
					() -> assertEquals(Map.of("step", "upload", "action", "ActionsUpload"), ids.get("s180")),
					() -> assertEquals(Map.of("step", "action", "name", "RunFromTabButton"), ids.get("s116:s125")),
					() -> assertEquals(Map.of("step", "tabSelect", "path", "Rows"), ids.get("s116:s129")),
					() -> assertEquals(Map.of("binding", "outcome", "widget", "textField"), ids.get("s102")),
					() -> assertTrue(entries.contains(Map.of("step", "action", "name", "RunFromToolbar")), "a toolbar action"),
					() -> assertTrue(entries.contains(Map.of("step", "save")), "Save"),
					() -> assertTrue(entries.contains(Map.of("step", "ok")), "OK"),
					() -> assertTrue(entries.contains(Map.of("step", "delete")), "Delete"),
					() -> assertTrue(entries.contains(Map.of("step", "action", "name", "DownloadOnce")), "a download"),
					() -> assertTrue(entries.contains(Map.of("binding", "disableUpload", "widget", "checkBox")), "a check box"),
					() -> assertTrue(entries.contains(Map.of("dataGrid", "rows")), "the data grid"),
					() -> assertTrue(entries.contains(Map.of("step", "dataGridNew", "binding", "rows")), "the data grid's New"),
					() -> assertTrue(entries.contains(Map.of("step", "dataGridZoom", "binding", "rows")), "the data grid's zoom"),
					() -> assertEquals(List.of("name", "hideLink", "disableRowAction", "outcome"), bindings(columns)),
					() -> assertEquals(Map.of("title", "Row action"), columns.get(4)));
	}

	/**
	 * A list page's map names the context's own list grid, and gives its columns. The list is a Vue list grid, whose
	 * toolbar and rows are made in the browser, so only its container is in the map.
	 */
	@Test
	void aListPageNamesItsOwnGrid() throws Exception {
		AbstractPersistence.get().setUser(SailUsers.bootstrapLikeUser(u));
		Map<String, Object> map = parse(PrimeFacesRecordingMap.list("kitchensink", "LookupDescription", null, null, "tablet", UserAgentType.tablet));
		Map<String, Map<String, Object>> ids = (Map<String, Map<String, Object>>) map.get("ids");
		Map<String, Object> grid = (Map<String, Object>) ((Map<String, Object>) map.get("grids")).get("kitchensink.LookupDescription");
		List<Map<String, Object>> columns = (List<Map<String, Object>>) grid.get("columns");

		assertAll(() -> assertEquals(Map.of("module", "kitchensink", "document", "LookupDescription"), map.get("list")),
					() -> assertEquals(Map.of("s0", Map.of("listGrid", "kitchensink.LookupDescription")), ids),
					() -> assertTrue(bindings(columns).contains("description"), "the description column: " + columns));
	}

	/**
	 * A list grid rendered as a PrimeFaces data table gives each column's header client id, which its sort request
	 * names, and the comparison its filter makes, as the executor filters it: contains for text, equals for a constant
	 * domain's combo, a boolean's check box and a date. A Vue list grid's columns have neither.
	 */
	@Test
	void aDataTablesColumnsHaveTheirIdsAndFilterOperators() throws Exception {
		AbstractPersistence.get().setUser(SailUsers.bootstrapLikeUser(u));
		Map<String, Object> map = parse(PrimeFacesRecordingMap.edit("kitchensink", "ListGridFixture", "tablet", UserAgentType.tablet));
		Map<String, Object> grids = (Map<String, Object>) map.get("grids");
		List<Map<String, Object>> faces = (List<Map<String, Object>>) ((Map<String, Object>) grids.get("kitchensink.qListGridFixtureFacesAttributes")).get("columns");
		List<Map<String, Object>> vue = (List<Map<String, Object>>) ((Map<String, Object>) grids.get("kitchensink.qListGridFixtureVueAttributes")).get("columns");
		Map<String, Map<String, Object>> byBinding = new java.util.HashMap<>();
		faces.forEach(column -> byBinding.put((String) column.get("binding"), column));

		assertAll(() -> assertEquals("contains", byBinding.get("text").get("operator")),
					() -> assertEquals("equals", byBinding.get("constantEnum").get("operator")),
					() -> assertEquals("equals", byBinding.get("booleanFlag").get("operator")),
					() -> assertEquals("equals", byBinding.get("date").get("operator")),
					() -> assertTrue(faces.stream().allMatch(column -> String.valueOf(column.get("id")).matches("s\\d+(:s\\d+)+")), "header ids: " + faces),
					() -> assertEquals(faces.size(), faces.stream().map(column -> column.get("id")).distinct().count(), "one id each: " + faces),
					() -> assertTrue(vue.stream().noneMatch(column -> column.containsKey("id") || column.containsKey("operator")), "no ids for a Vue grid: " + vue));
	}

	/**
	 * A list model column with neither a name nor a binding (one a list model makes in code) has no data table field:
	 * it is listed with its title and no header id, and the other columns keep theirs.
	 */
	@Test
	@SuppressWarnings("static-method")
	void aColumnWithoutANameOrBindingIsListedWithoutAnId() {
		MetaDataQueryProjectedColumnImpl unnamed = new MetaDataQueryProjectedColumnImpl();
		unnamed.setDisplayName("Made in code");
		MetaDataQueryProjectedColumnImpl bound = new MetaDataQueryProjectedColumnImpl();
		bound.setBinding("description");
		ListModel<Bean> model = org.mockito.Mockito.mock(ListModel.class);
		org.mockito.Mockito.when(model.getColumns()).thenReturn(List.of(unnamed, bound));
		org.mockito.Mockito.when(model.determineColumnTitle(unnamed)).thenReturn("Made in code");
		org.mockito.Mockito.when(model.determineColumnTitle(bound)).thenReturn("Description");
		List<Map<String, Object>> columns;
		try (MockFacesContext ignored = MockFacesContext.replacingCurrent()) {
			DataTable table = new DataTable();
			table.setId("t");
			Column column = new Column();
			column.setId("c");
			column.setField("description");
			column.setFilterable(false);
			table.getChildren().add(column);
			columns = PrimeFacesRecordingMap.listGridColumns(model, table);
		}
		assertAll(() -> assertEquals(Map.of("title", "Made in code"), columns.get(0)),
					() -> assertEquals("t:c", columns.get(1).get("id")),
					() -> assertEquals("description", columns.get(1).get("binding")));
	}

	/**
	 * The map is built with the user's real roles: a recorder without DevOps gets the PrimeFaces Control Panel's
	 * recording controls and none of its DevOps actions, and a DevOps user without the recorder role the reverse.
	 */
	@Test
	void theMapFollowsTheUsersRoles() throws Exception {
		AbstractPersistence.get().setUser(SailUsers.userWithRoles(u, List.of("admin.BasicUser", "admin.SAILRecorder")));
		Collection<Map<String, Object>> recorder = controlPanelEntries();
		AbstractPersistence.get().setUser(SailUsers.userWithRoles(u, List.of("admin.BasicUser", "admin.DevOps")));
		Collection<Map<String, Object>> devOps = controlPanelEntries();

		Map<String, Object> start = Map.of("step", "action", "name", "StartSAILRecording");
		Map<String, Object> stop = Map.of("step", "action", "name", "StopSAILRecording");
		Map<String, Object> download = Map.of("step", "action", "name", "DownloadSAILRecording");
		Map<String, Object> generate = Map.of("step", "action", "name", "GenerateTestData");
		assertAll(() -> assertTrue(recorder.containsAll(List.of(start, stop, download)), "the recorder's controls: " + recorder),
					() -> assertFalse(recorder.contains(generate), "no DevOps action for the recorder"),
					() -> assertTrue(devOps.contains(generate), "DevOps actions: " + devOps),
					() -> assertFalse(devOps.contains(start), "no recording control without the recorder role"));
	}

	/**
	 * A document with a separate create view (User's wizard): the page renders both views, so the map names the create
	 * view's elements too, as a new record shows them.
	 */
	@Test
	void aCreateViewIsMappedToo() throws Exception {
		AbstractPersistence.get().setUser(SailUsers.bootstrapLikeUser(u));
		Map<String, Object> map = parse(PrimeFacesRecordingMap.edit("admin", "User", "tablet", UserAgentType.tablet));
		Collection<Map<String, Object>> entries = ((Map<String, Map<String, Object>>) map.get("ids")).values();
		assertAll(() -> assertTrue(entries.contains(Map.of("step", "action", "name", "Next")), "the create view's Next: " + entries),
					() -> assertTrue(entries.contains(Map.of("step", "save")), "the edit view's Save: " + entries));
	}

	private static Collection<Map<String, Object>> controlPanelEntries() throws Exception {
		Map<String, Object> map = parse(PrimeFacesRecordingMap.edit("admin", "ControlPanel", "external", UserAgentType.desktop));
		return ((Map<String, Map<String, Object>>) map.get("ids")).values();
	}

	private static Map<String, Object> parse(String json) throws Exception {
		if (JSON.unmarshall(json) instanceof Map<?, ?> result) {
			return (Map<String, Object>) result;
		}
		throw new AssertionError("Not a JSON object: " + json);
	}

	private static List<Object> bindings(List<Map<String, Object>> columns) {
		List<Object> result = new ArrayList<>(columns.size());
		for (Map<String, Object> column : columns) {
			Object binding = column.get("binding");
			if (binding != null) {
				result.add(binding);
			}
		}
		return result;
	}
}
