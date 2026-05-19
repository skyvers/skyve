package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.metadata.controller.ImplicitActionName;

/**
 * Tests for {@link FluentActions} add/find/remove methods.
 */
@SuppressWarnings("static-method")
class FluentActionsTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentActions().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		Actions actions = new Actions();
		assertThat(new FluentActions(actions).get(), is(actions));
	}

	@Test
	void widgetIdSetsValue() {
		assertThat(new FluentActions().widgetId("w1").get().getWidgetId(), is("w1"));
	}

	// ---- AddAction ----

	@Test
	void addAddActionAddsAction() {
		FluentActions fa = new FluentActions().addAddAction(new FluentAddAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findAddActionReturnsMatch() {
		FluentActions fa = new FluentActions().addAddAction(new FluentAddAction());
		assertThat(fa.findAddAction(), is(notNullValue()));
	}

	@Test
	void findAddActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findAddAction(), is(nullValue()));
	}

	@Test
	void findNamedAddActionReturnsMatch() {
		FluentActions fa = new FluentActions().addAddAction(new FluentAddAction().name("myAdd"));
		assertThat(fa.findNamedAddAction("myAdd"), is(notNullValue()));
	}

	@Test
	void findNamedAddActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedAddAction("missing"), is(nullValue()));
	}

	// ---- CancelAction ----

	@Test
	void addCancelActionAddsAction() {
		FluentActions fa = new FluentActions().addCancelAction(new FluentCancelAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findCancelActionReturnsMatch() {
		FluentActions fa = new FluentActions().addCancelAction(new FluentCancelAction());
		assertThat(fa.findCancelAction(), is(notNullValue()));
	}

	@Test
	void findCancelActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findCancelAction(), is(nullValue()));
	}

	@Test
	void findNamedCancelActionReturnsMatch() {
		FluentActions fa = new FluentActions().addCancelAction(new FluentCancelAction().name("cancel1"));
		assertThat(fa.findNamedCancelAction("cancel1"), is(notNullValue()));
	}

	// ---- DefaultsAction ----

	@Test
	void addDefaultsActionAddsAction() {
		FluentActions fa = new FluentActions().addDefaultsAction(new FluentDefaultsAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findDefaultsActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDefaultsAction(new FluentDefaultsAction());
		assertThat(fa.findDefaultsAction(), is(notNullValue()));
	}

	@Test
	void findDefaultsActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findDefaultsAction(), is(nullValue()));
	}

	// ---- NewAction ----

	@Test
	void addNewActionAddsAction() {
		FluentActions fa = new FluentActions().addNewAction(new FluentNewAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findNewActionReturnsMatch() {
		FluentActions fa = new FluentActions().addNewAction(new FluentNewAction());
		assertThat(fa.findNewAction(), is(notNullValue()));
	}

	@Test
	void findNewActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNewAction(), is(nullValue()));
	}

	// ---- RemoveAction ----

	@Test
	void addRemoveActionAddsAction() {
		FluentActions fa = new FluentActions().addRemoveAction(new FluentRemoveAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findRemoveActionReturnsMatch() {
		FluentActions fa = new FluentActions().addRemoveAction(new FluentRemoveAction());
		assertThat(fa.findRemoveAction(), is(notNullValue()));
	}

	@Test
	void findRemoveActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findRemoveAction(), is(nullValue()));
	}

	// ---- DeleteAction ----

	@Test
	void addDeleteActionAddsAction() {
		FluentActions fa = new FluentActions().addDeleteAction(new FluentDeleteAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findDeleteActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDeleteAction(new FluentDeleteAction());
		assertThat(fa.findDeleteAction(), is(notNullValue()));
	}

	@Test
	void findDeleteActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findDeleteAction(), is(nullValue()));
	}

	// ---- OKAction ----

	@Test
	void addOKActionAddsAction() {
		FluentActions fa = new FluentActions().addOKAction(new FluentOKAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findOKActionReturnsMatch() {
		FluentActions fa = new FluentActions().addOKAction(new FluentOKAction());
		assertThat(fa.findOKAction(), is(notNullValue()));
	}

	@Test
	void findOKActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findOKAction(), is(nullValue()));
	}

	// ---- PrintAction ----

	@Test
	void addPrintActionAddsAction() {
		FluentActions fa = new FluentActions().addPrintAction(new FluentPrintAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findPrintActionReturnsMatch() {
		FluentActions fa = new FluentActions().addPrintAction(new FluentPrintAction());
		assertThat(fa.findPrintAction(), is(notNullValue()));
	}

	@Test
	void findPrintActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findPrintAction(), is(nullValue()));
	}

	// ---- SaveAction ----

	@Test
	void addSaveActionAddsAction() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findSaveActionReturnsMatch() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction());
		assertThat(fa.findSaveAction(), is(notNullValue()));
	}

	@Test
	void findSaveActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findSaveAction(), is(nullValue()));
	}

	// ---- ZoomOutAction ----

	@Test
	void addZoomOutActionAddsAction() {
		FluentActions fa = new FluentActions().addZoomOutAction(new FluentZoomOutAction());
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findNamedZoomOutActionReturnsMatch() {
		FluentActions fa = new FluentActions().addZoomOutAction(new FluentZoomOutAction().name("zoom1"));
		assertThat(fa.findNamedZoomOutAction("zoom1"), is(notNullValue()));
	}

	@Test
	void findNamedZoomOutActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedZoomOutAction("missing"), is(nullValue()));
	}

	// ---- BizExportAction ----

	@Test
	void addBizExportActionAddsAction() {
		FluentActions fa = new FluentActions().addBizExportAction(new FluentBizExportAction().className("com.MyExport"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findBizExportActionByClassNameReturnsMatch() {
		FluentActions fa = new FluentActions().addBizExportAction(new FluentBizExportAction().className("com.MyExport"));
		assertThat(fa.findBizExportAction("com.MyExport"), is(notNullValue()));
	}

	@Test
	void findBizExportActionByClassNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findBizExportAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedBizExportActionReturnsMatch() {
		FluentActions fa = new FluentActions().addBizExportAction(new FluentBizExportAction().name("exportAction"));
		assertThat(fa.findNamedBizExportAction("exportAction"), is(notNullValue()));
	}

	// ---- BizImportAction ----

	@Test
	void addBizImportActionAddsAction() {
		FluentActions fa = new FluentActions().addBizImportAction(new FluentBizImportAction().className("com.MyImport"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findBizImportActionByClassNameReturnsMatch() {
		FluentActions fa = new FluentActions().addBizImportAction(new FluentBizImportAction().className("com.MyImport"));
		assertThat(fa.findBizImportAction("com.MyImport"), is(notNullValue()));
	}

	@Test
	void findBizImportActionByClassNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findBizImportAction("missing"), is(nullValue()));
	}

	// ---- CustomAction ----

	@Test
	void addCustomActionAddsAction() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().className("com.MyAction"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findCustomActionByClassNameReturnsMatch() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().className("com.MyAction"));
		assertThat(fa.findCustomAction("com.MyAction"), is(notNullValue()));
	}

	@Test
	void findCustomActionByClassNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findCustomAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedCustomActionReturnsMatch() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().name("customAction1"));
		assertThat(fa.findNamedCustomAction("customAction1"), is(notNullValue()));
	}

	// ---- DownloadAction ----

	@Test
	void addDownloadActionAddsAction() {
		FluentActions fa = new FluentActions().addDownloadAction(new FluentDownloadAction().className("com.MyDownload"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findDownloadActionByClassNameReturnsMatch() {
		FluentActions fa = new FluentActions().addDownloadAction(new FluentDownloadAction().className("com.MyDownload"));
		assertThat(fa.findDownloadAction("com.MyDownload"), is(notNullValue()));
	}

	@Test
	void findDownloadActionByClassNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findDownloadAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedDownloadActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDownloadAction(new FluentDownloadAction().name("dl1"));
		assertThat(fa.findNamedDownloadAction("dl1"), is(notNullValue()));
	}

	// ---- UploadAction ----

	@Test
	void addUploadActionAddsAction() {
		FluentActions fa = new FluentActions().addUploadAction(new FluentUploadAction().className("com.MyUpload"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findUploadActionByClassNameReturnsMatch() {
		FluentActions fa = new FluentActions().addUploadAction(new FluentUploadAction().className("com.MyUpload"));
		assertThat(fa.findUploadAction("com.MyUpload"), is(notNullValue()));
	}

	@Test
	void findUploadActionByClassNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findUploadAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedUploadActionReturnsMatch() {
		FluentActions fa = new FluentActions().addUploadAction(new FluentUploadAction().name("upload1"));
		assertThat(fa.findNamedUploadAction("upload1"), is(notNullValue()));
	}

	// ---- ReportAction ----

	@Test
	void addReportActionAddsAction() {
		FluentActions fa = new FluentActions().addReportAction(new FluentReportAction().reportName("MyReport"));
		assertEquals(1, fa.get().getActions().size());
	}

	@Test
	void findReportActionByReportNameReturnsMatch() {
		FluentActions fa = new FluentActions().addReportAction(new FluentReportAction().reportName("MyReport"));
		assertThat(fa.findReportAction("MyReport"), is(notNullValue()));
	}

	@Test
	void findReportActionByReportNameReturnsNullWhenMissing() {
		assertThat(new FluentActions().findReportAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedReportActionReturnsMatch() {
		FluentActions fa = new FluentActions().addReportAction(new FluentReportAction().name("report1"));
		assertThat(fa.findNamedReportAction("report1"), is(notNullValue()));
	}

	@Test
	void findNamedReportActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedReportAction("missing"), is(nullValue()));
	}

	@Test
	void removeReportActionRemovesIt() {
		FluentActions fa = new FluentActions().addReportAction(new FluentReportAction().reportName("R1"));
		fa.removeReportAction("R1");
		assertEquals(0, fa.get().getActions().size());
	}

	// ---- indexed add overloads ----

	@Test
	void addAddActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions()
				.addAddAction(new FluentAddAction().name("first"))
				.addAddAction(new FluentAddAction().name("third"));
		fa.addAddAction(1, new FluentAddAction().name("second"));
		assertThat(fa.get().getActions().get(1).getName(), is("second"));
	}

	@Test
	void addCancelActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addCancelAction(new FluentCancelAction().name("a"));
		fa.addCancelAction(0, new FluentCancelAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addSaveActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction().name("a"));
		fa.addSaveAction(0, new FluentSaveAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	// ---- remove / clear ----

	@Test
	void removeNamedActionRemovesIt() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction().name("save1"));
		fa.removeNamedAction("save1");
		assertEquals(0, fa.get().getActions().size());
	}

	@Test
	void removeImplicitActionRemovesIt() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction());
		fa.removeImplicitAction(ImplicitActionName.Save);
		assertEquals(0, fa.get().getActions().size());
	}

	@Test
	void removeClassActionRemovesIt() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().className("com.A"));
		fa.removeClassAction("com.A");
		assertEquals(0, fa.get().getActions().size());
	}

	@Test
	void removeActionAtIndexRemovesIt() {
		FluentActions fa = new FluentActions()
				.addSaveAction(new FluentSaveAction().name("s1"))
				.addSaveAction(new FluentSaveAction().name("s2"));
		fa.removeAction(0);
		assertEquals(1, fa.get().getActions().size());
		assertThat(fa.get().getActions().get(0).getName(), is("s2"));
	}

	@Test
	void clearActionsRemovesAll() {
		FluentActions fa = new FluentActions()
				.addSaveAction(new FluentSaveAction())
				.addDeleteAction(new FluentDeleteAction());
		fa.clearActions();
		assertEquals(0, fa.get().getActions().size());
	}

	@Test
	void findNamedDefaultsActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDefaultsAction(new FluentDefaultsAction().name("def1"));
		assertThat(fa.findNamedDefaultsAction("def1"), is(notNullValue()));
	}

	@Test
	void findNamedNewActionReturnsMatch() {
		FluentActions fa = new FluentActions().addNewAction(new FluentNewAction().name("new1"));
		assertThat(fa.findNamedNewAction("new1"), is(notNullValue()));
	}

	@Test
	void findNamedRemoveActionReturnsMatch() {
		FluentActions fa = new FluentActions().addRemoveAction(new FluentRemoveAction().name("rem1"));
		assertThat(fa.findNamedRemoveAction("rem1"), is(notNullValue()));
	}

	@Test
	void findNamedDeleteActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDeleteAction(new FluentDeleteAction().name("del1"));
		assertThat(fa.findNamedDeleteAction("del1"), is(notNullValue()));
	}

	@Test
	void findNamedOKActionReturnsMatch() {
		FluentActions fa = new FluentActions().addOKAction(new FluentOKAction().name("ok1"));
		assertThat(fa.findNamedOKAction("ok1"), is(notNullValue()));
	}

	@Test
	void findNamedPrintActionReturnsMatch() {
		FluentActions fa = new FluentActions().addPrintAction(new FluentPrintAction().name("print1"));
		assertThat(fa.findNamedPrintAction("print1"), is(notNullValue()));
	}

	@Test
	void findNamedSaveActionReturnsMatch() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction().name("save1"));
		assertThat(fa.findNamedSaveAction("save1"), is(notNullValue()));
	}

	@Test
	void findNamedBizImportActionReturnsMatch() {
		FluentActions fa = new FluentActions().addBizImportAction(new FluentBizImportAction().name("import1"));
		assertThat(fa.findNamedBizImportAction("import1"), is(notNullValue()));
	}

        @Test
        void addAddActionAtIndexInsertsAtCorrectPosition() {
                FluentActions fa = new FluentActions()
                        .addAddAction(new FluentAddAction())
                        .addAddAction(0, new FluentAddAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void addDefaultsActionAtIndex() {
                FluentActions fa = new FluentActions()
                        .addDefaultsAction(new FluentDefaultsAction())
                        .addDefaultsAction(0, new FluentDefaultsAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void addNewActionAtIndex() {
                FluentActions fa = new FluentActions()
                        .addNewAction(new FluentNewAction())
                        .addNewAction(0, new FluentNewAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void addDeleteActionAtIndex() {
                FluentActions fa = new FluentActions()
                        .addDeleteAction(new FluentDeleteAction())
                        .addDeleteAction(0, new FluentDeleteAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void addOKActionAtIndex() {
                FluentActions fa = new FluentActions()
                        .addOKAction(new FluentOKAction())
                        .addOKAction(0, new FluentOKAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void addPrintActionAtIndex() {
                FluentActions fa = new FluentActions()
                        .addPrintAction(new FluentPrintAction())
                        .addPrintAction(0, new FluentPrintAction());
                assertEquals(2, fa.get().getActions().size());
        }

        @Test
        void fromPopulatesActionsFromCollection() {
                java.util.List<org.skyve.metadata.view.Action> actions = new java.util.ArrayList<>();
                FluentActions fa = new FluentActions().from("wid", actions);
                assertThat(fa.get().getWidgetId(), is("wid"));
        }

	@Test
	void addRemoveActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addRemoveAction(new FluentRemoveAction())
				.addRemoveAction(0, new FluentRemoveAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addZoomOutActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addZoomOutAction(new FluentZoomOutAction())
				.addZoomOutAction(0, new FluentZoomOutAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addBizExportActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addBizExportAction(new FluentBizExportAction())
				.addBizExportAction(0, new FluentBizExportAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addBizImportActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addBizImportAction(new FluentBizImportAction())
				.addBizImportAction(0, new FluentBizImportAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addCustomActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addCustomAction(new FluentCustomAction())
				.addCustomAction(0, new FluentCustomAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addDownloadActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addDownloadAction(new FluentDownloadAction())
				.addDownloadAction(0, new FluentDownloadAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addUploadActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addUploadAction(new FluentUploadAction())
				.addUploadAction(0, new FluentUploadAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addReportActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addReportAction(new FluentReportAction())
				.addReportAction(0, new FluentReportAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addCancelActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addCancelAction(new FluentCancelAction())
				.addCancelAction(0, new FluentCancelAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void addSaveActionAtIndex() {
		FluentActions fa = new FluentActions()
				.addSaveAction(new FluentSaveAction())
				.addSaveAction(0, new FluentSaveAction());
		assertEquals(2, fa.get().getActions().size());
	}

	@Test
	void findNamedCancelActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedCancelAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedNewActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedNewAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedRemoveActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedRemoveAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedDeleteActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedDeleteAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedOKActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedOKAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedSaveActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedSaveAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedBizExportActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedBizExportAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedCustomActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedCustomAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedDownloadActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedDownloadAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedUploadActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedUploadAction("missing"), is(nullValue()));
	}

	@Test
	void findZoomOutActionReturnsNullWhenNoZoomOutAction() {
		assertThat(new FluentActions().findZoomOutAction(), is(nullValue()));
	}

}
