package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(1));
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
		assertThat(fa.get().getActions().size(), is(0));
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
		assertThat(fa.get().getActions().size(), is(0));
	}

	@Test
	void removeImplicitActionRemovesIt() {
		FluentActions fa = new FluentActions().addSaveAction(new FluentSaveAction());
		fa.removeImplicitAction(ImplicitActionName.Save);
		assertThat(fa.get().getActions().size(), is(0));
	}

	@Test
	void removeClassActionRemovesIt() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().className("com.A"));
		fa.removeClassAction("com.A");
		assertThat(fa.get().getActions().size(), is(0));
	}

	@Test
	void removeActionAtIndexRemovesIt() {
		FluentActions fa = new FluentActions()
				.addSaveAction(new FluentSaveAction().name("s1"))
				.addSaveAction(new FluentSaveAction().name("s2"));
		fa.removeAction(0);
		assertThat(fa.get().getActions().size(), is(1));
		assertThat(fa.get().getActions().get(0).getName(), is("s2"));
	}

	@Test
	void clearActionsRemovesAll() {
		FluentActions fa = new FluentActions()
				.addSaveAction(new FluentSaveAction())
				.addDeleteAction(new FluentDeleteAction());
		fa.clearActions();
		assertThat(fa.get().getActions().size(), is(0));
	}
}
