package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.DownloadAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.PrintAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
class FluentActionsCoverageTest {
	@Test
	void fluentActionFromMapsAllTargetActionTypes() {
		assertFromType(new AddAction(), FluentAddAction.class);
		assertFromType(new CancelAction(), FluentCancelAction.class);
		assertFromType(new DefaultsAction(), FluentDefaultsAction.class);
		assertFromType(new NewAction(), FluentNewAction.class);
		assertFromType(new RemoveAction(), FluentRemoveAction.class);
		assertFromType(new DeleteAction(), FluentDeleteAction.class);
		assertFromType(new OKAction(), FluentOKAction.class);
		assertFromType(new SaveAction(), FluentSaveAction.class);
		assertFromType(new ZoomOutAction(), FluentZoomOutAction.class);
		assertFromType(new BizExportAction(), FluentBizExportAction.class);
		assertFromType(new BizImportAction(), FluentBizImportAction.class);
		assertFromType(new CustomAction(), FluentCustomAction.class);
		assertFromType(new DownloadAction(), FluentDownloadAction.class);
		assertFromType(new UploadAction(), FluentUploadAction.class);
		assertFromType(new ReportAction(), FluentReportAction.class);
		assertFromType(new PrintAction(), FluentPrintAction.class);
	}

	@Test
	void fluentActionFromActionUsesActionImplConversion() {
		ActionImpl add = new ActionImpl();
		add.setImplicitName(ImplicitActionName.Add);
		add.setName("addByAction");
		FluentAction<?> fluentAdd = FluentAction.from(add);
		assertThat(fluentAdd, is(instanceOf(FluentAddAction.class)));

		ActionImpl custom = new ActionImpl();
		custom.setName("customByAction");
		custom.setResourceName("modules.actions.CustomActionClass");
		FluentAction<?> fluentCustom = FluentAction.from(custom);
		assertThat(fluentCustom, is(instanceOf(FluentCustomAction.class)));
		assertThat(((FluentCustomAction) fluentCustom).get().getClassName(), is("modules.actions.CustomActionClass"));
	}

	@Test
	void actionSubclassConstructorsGetAndFromRetainActionInstances() {
		AddAction add = new AddAction();
		CancelAction cancel = new CancelAction();
		DefaultsAction defaults = new DefaultsAction();
		NewAction newAction = new NewAction();
		RemoveAction remove = new RemoveAction();
		DeleteAction delete = new DeleteAction();
		OKAction ok = new OKAction();
		SaveAction save = new SaveAction();
		ZoomOutAction zoomOut = new ZoomOutAction();
		BizExportAction export = new BizExportAction();
		BizImportAction bizImport = new BizImportAction();
		CustomAction custom = new CustomAction();
		DownloadAction download = new DownloadAction();
		UploadAction upload = new UploadAction();
		ReportAction report = new ReportAction();
		PrintAction print = new PrintAction();

		assertThat(new FluentAddAction(add).from(add).get(), is(add));
		assertThat(new FluentCancelAction(cancel).from(cancel).get(), is(cancel));
		assertThat(new FluentDefaultsAction(defaults).from(defaults).get(), is(defaults));
		assertThat(new FluentNewAction(newAction).from(newAction).get(), is(newAction));
		assertThat(new FluentRemoveAction(remove).from(remove).get(), is(remove));
		assertThat(new FluentDeleteAction(delete).from(delete).get(), is(delete));
		assertThat(new FluentOKAction(ok).from(ok).get(), is(ok));
		assertThat(new FluentSaveAction(save).from(save).get(), is(save));
		assertThat(new FluentZoomOutAction(zoomOut).from(zoomOut).get(), is(zoomOut));
		assertThat(new FluentBizExportAction(export).from(export).get(), is(export));
		assertThat(new FluentBizImportAction(bizImport).from(bizImport).get(), is(bizImport));
		assertThat(new FluentCustomAction(custom).from(custom).get(), is(custom));
		assertThat(new FluentDownloadAction(download).from(download).get(), is(download));
		assertThat(new FluentUploadAction(upload).from(upload).get(), is(upload));
		assertThat(new FluentReportAction(report).from(report).get(), is(report));
		assertThat(new FluentPrintAction(print).from(print).get(), is(print));
	}

	@Test
	void fluentActionsFromCollectionAndWidgetIdMapsActions() {
		ActionImpl add = new ActionImpl();
		add.setImplicitName(ImplicitActionName.Add);
		add.setName("addFromCollection");

		ActionImpl custom = new ActionImpl();
		custom.setName("customFromCollection");
		custom.setResourceName("modules.actions.ImportAction");

		FluentActions fluentActions = new FluentActions().from("actionsWidget", List.of(add, custom));
		assertThat(fluentActions.get().getWidgetId(), is("actionsWidget"));
		assertEquals(2, fluentActions.get().getActions().size());
		assertThat(fluentActions.findAddAction(), is(notNullValue()));
		assertThat(fluentActions.findCustomAction("modules.actions.ImportAction"), is(notNullValue()));
	}

	@Test
	void fluentActionsAddFindRemoveAndClearWork() {
		FluentActions fluentActions = new FluentActions(new Actions()).widgetId("widgetA");
		fluentActions.addAddAction(new FluentAddAction().name("addNamed"));
		fluentActions.addCancelAction(new FluentCancelAction().name("cancelNamed"));
		fluentActions.addDefaultsAction(new FluentDefaultsAction().name("defaultsNamed"));
		fluentActions.addNewAction(new FluentNewAction().name("newNamed"));
		fluentActions.addRemoveAction(new FluentRemoveAction().name("removeNamed"));
		fluentActions.addDeleteAction(new FluentDeleteAction().name("deleteNamed"));
		fluentActions.addOKAction(new FluentOKAction().name("okNamed"));
		fluentActions.addSaveAction(new FluentSaveAction().name("saveNamed"));
		fluentActions.addZoomOutAction(new FluentZoomOutAction().name("zoomOutNamed"));
		fluentActions.addBizExportAction(new FluentBizExportAction().name("bizExportNamed").className("ExportAction"));
		fluentActions.addBizImportAction(new FluentBizImportAction().name("bizImportNamed").className("ImportAction"));
		fluentActions.addCustomAction(new FluentCustomAction().name("customNamed").className("CustomAction"));
		fluentActions.addDownloadAction(new FluentDownloadAction().name("downloadNamed").className("DownloadAction"));
		fluentActions.addUploadAction(new FluentUploadAction().name("uploadNamed").className("UploadAction"));
		fluentActions.addReportAction(new FluentReportAction().name("reportNamed").reportName("salesReport"));
		fluentActions.addPrintAction(new FluentPrintAction().name("printNamed"));

		assertThat(fluentActions.findNamedAddAction("addNamed"), is(notNullValue()));
		assertThat(fluentActions.findCancelAction(), is(notNullValue()));
		assertThat(fluentActions.findDefaultsAction(), is(notNullValue()));
		assertThat(fluentActions.findNewAction(), is(notNullValue()));
		assertThat(fluentActions.findRemoveAction(), is(notNullValue()));
		assertThat(fluentActions.findDeleteAction(), is(notNullValue()));
		assertThat(fluentActions.findOKAction(), is(notNullValue()));
		assertThat(fluentActions.findSaveAction(), is(notNullValue()));
		assertThat(fluentActions.findNamedZoomOutAction("zoomOutNamed"), is(notNullValue()));
		assertThat(fluentActions.findBizExportAction("ExportAction"), is(notNullValue()));
		assertThat(fluentActions.findBizImportAction("ImportAction"), is(notNullValue()));
		assertThat(fluentActions.findCustomAction("CustomAction"), is(notNullValue()));
		assertThat(fluentActions.findDownloadAction("DownloadAction"), is(notNullValue()));
		assertThat(fluentActions.findUploadAction("UploadAction"), is(notNullValue()));
		assertThat(fluentActions.findReportAction("salesReport"), is(notNullValue()));
		assertThat(fluentActions.findPrintAction(), is(notNullValue()));

		int beforeRemoval = fluentActions.get().getActions().size();
		fluentActions.removeNamedAction("printNamed");
		assertThat(fluentActions.findNamedPrintAction("printNamed"), is(nullValue()));
		fluentActions.removeImplicitAction(ImplicitActionName.Cancel);
		assertThat(fluentActions.findCancelAction(), is(nullValue()));
		fluentActions.removeClassAction("ImportAction");
		assertThat(fluentActions.findBizImportAction("ImportAction"), is(nullValue()));
		fluentActions.removeReportAction("salesReport");
		assertThat(fluentActions.findReportAction("salesReport"), is(nullValue()));
		fluentActions.removeAction(0);
		assertEquals(beforeRemoval - 5, fluentActions.get().getActions().size());
		fluentActions.clearActions();
		assertEquals(0, fluentActions.get().getActions().size());
	}

	private static void assertFromType(ActionMetaData action, Class<?> expectedType) {
		FluentAction<?> fluent = FluentAction.from(action);
		assertThat(fluent, is(instanceOf(expectedType)));
	}
}
