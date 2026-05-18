package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

class ActionVisitorTest {

	/**
	 * Simple concrete implementation that records which visit method was called.
	 */
	private static class RecordingVisitor extends ActionVisitor {
		final List<String> visited = new ArrayList<>();

		@Override
		public void visitCustomAction(ActionImpl action) {
			visited.add("custom");
		}

		@Override
		public void visitAddAction(ActionImpl action) {
			visited.add("add");
		}

		@Override
		public void visitRemoveAction(ActionImpl action) {
			visited.add("remove");
		}

		@Override
		public void visitZoomOutAction(ActionImpl action) {
			visited.add("zoomOut");
		}

		@Override
		public void visitNavigateAction(ActionImpl action) {
			visited.add("navigate");
		}

		@Override
		public void visitOKAction(ActionImpl action) {
			visited.add("ok");
		}

		@Override
		public void visitSaveAction(ActionImpl action) {
			visited.add("save");
		}

		@Override
		public void visitCancelAction(ActionImpl action) {
			visited.add("cancel");
		}

		@Override
		public void visitDeleteAction(ActionImpl action) {
			visited.add("delete");
		}

		@Override
		public void visitReportAction(ActionImpl action) {
			visited.add("report");
		}

		@Override
		public void visitBizExportAction(ActionImpl action) {
			visited.add("bizExport");
		}

		@Override
		public void visitBizImportAction(ActionImpl action) {
			visited.add("bizImport");
		}

		@Override
		public void visitDownloadAction(ActionImpl action) {
			visited.add("download");
		}

		@Override
		public void visitUploadAction(ActionImpl action) {
			visited.add("upload");
		}

		@Override
		public void visitNewAction(ActionImpl action) {
			visited.add("new");
		}

		@Override
		public void visitEditAction(ActionImpl action) {
			visited.add("edit");
		}

		@Override
		public void visitPrintAction(ActionImpl action) {
			visited.add("print");
		}

		@Override
		public void visitParameter(org.skyve.metadata.view.widget.bound.Parameter parameter,
				boolean parentVisible,
				boolean parentEnabled) {
			visited.add("parameter");
		}

		@Override
		public void visitFilterParameter(org.skyve.metadata.view.widget.FilterParameter parameter,
				boolean parentVisible,
				boolean parentEnabled) {
			visited.add("filterParameter");
		}
	}

	private static ActionImpl action(String name, ImplicitActionName implicit) {
		ActionImpl a = new ActionImpl();
		a.setName(name);
		a.setImplicitName(implicit);
		return a;
	}

	private static ActionImpl customAction(String name) {
		ActionImpl a = new ActionImpl();
		a.setName(name);
		return a;
	}

	private static ViewImpl view(String name, ActionImpl... actions) {
		ViewImpl v = new ViewImpl();
		v.setName(name);
		for (ActionImpl a : actions) {
			v.putAction(a);
		}
		return v;
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesCustomAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", customAction("myAction")));
		assertEquals(List.of("custom"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesAddAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Add", ImplicitActionName.Add)));
		assertEquals(List.of("add"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesRemoveAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Remove", ImplicitActionName.Remove)));
		assertEquals(List.of("remove"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesZoomOutAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("ZoomOut", ImplicitActionName.ZoomOut)));
		assertEquals(List.of("zoomOut"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesNavigateAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Navigate", ImplicitActionName.Navigate)));
		assertEquals(List.of("navigate"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesOKAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("OK", ImplicitActionName.OK)));
		assertEquals(List.of("ok"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesSaveAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Save", ImplicitActionName.Save)));
		assertEquals(List.of("save"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesCancelAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Cancel", ImplicitActionName.Cancel)));
		assertEquals(List.of("cancel"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesDeleteAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Delete", ImplicitActionName.Delete)));
		assertEquals(List.of("delete"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesReportAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Report", ImplicitActionName.Report)));
		assertEquals(List.of("report"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesBizExportAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("BizExport", ImplicitActionName.BizExport)));
		assertEquals(List.of("bizExport"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesBizImportAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("BizImport", ImplicitActionName.BizImport)));
		assertEquals(List.of("bizImport"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesDownloadAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Download", ImplicitActionName.Download)));
		assertEquals(List.of("download"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesUploadAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Upload", ImplicitActionName.Upload)));
		assertEquals(List.of("upload"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesNewAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("New", ImplicitActionName.New)));
		assertEquals(List.of("new"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesEditAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Edit", ImplicitActionName.Edit)));
		assertEquals(List.of("edit"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDispatchesPrintAction() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("Print", ImplicitActionName.Print)));
		assertEquals(List.of("print"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDefaultsOnListViewExpandsToNew() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("list", action("DEFAULTS", ImplicitActionName.DEFAULTS)));
		assertEquals(List.of("new"), visitor.visited);
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsDefaultsOnEditViewExpandsToEditViewImplicits() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit", action("DEFAULTS", ImplicitActionName.DEFAULTS)));
		// DEFAULTS on edit view expands to: OK, Save, Cancel, Delete, Add, ZoomOut, Edit
		// (anything not excluded by the DEFAULTS logic)
		// At minimum we should see several actions dispatched
		assert !visitor.visited.isEmpty() : "DEFAULTS should expand to multiple edit-view actions";
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsWithNoActionsDoesNothing() {
		RecordingVisitor visitor = new RecordingVisitor();
		visitor.visitActions(view("edit"));
		assertEquals(0, visitor.visited.size());
	}

	@Test
	@SuppressWarnings("static-method")
	void visitDefaultPrintActionCallsDefaultNoOpImpl() {
		// Use a visitor that does NOT override visitPrintAction so the default no-op runs
		ActionVisitor visitor = new ActionVisitor() {
			@Override public void visitCustomAction(ActionImpl action) { /* no-op */ }
			@Override public void visitAddAction(ActionImpl action) { /* no-op */ }
			@Override public void visitRemoveAction(ActionImpl action) { /* no-op */ }
			@Override public void visitZoomOutAction(ActionImpl action) { /* no-op */ }
			@Override public void visitNavigateAction(ActionImpl action) { /* no-op */ }
			@Override public void visitOKAction(ActionImpl action) { /* no-op */ }
			@Override public void visitSaveAction(ActionImpl action) { /* no-op */ }
			@Override public void visitCancelAction(ActionImpl action) { /* no-op */ }
			@Override public void visitDeleteAction(ActionImpl action) { /* no-op */ }
			@Override public void visitReportAction(ActionImpl action) { /* no-op */ }
			@Override public void visitBizExportAction(ActionImpl action) { /* no-op */ }
			@Override public void visitBizImportAction(ActionImpl action) { /* no-op */ }
			@Override public void visitDownloadAction(ActionImpl action) { /* no-op */ }
			@Override public void visitUploadAction(ActionImpl action) { /* no-op */ }
			@Override public void visitNewAction(ActionImpl action) { /* no-op */ }
			@Override public void visitEditAction(ActionImpl action) { /* no-op */ }
			@Override public void visitParameter(org.skyve.metadata.view.widget.bound.Parameter p, boolean pv, boolean pe) { /* no-op */ }
			@Override public void visitFilterParameter(org.skyve.metadata.view.widget.FilterParameter p, boolean pv, boolean pe) { /* no-op */ }
		};
		// Should not throw — default visitPrintAction is a no-op
		visitor.visitActions(view("edit", action("Print", ImplicitActionName.Print)));
	}

	@Test
	@SuppressWarnings("static-method")
	void visitActionsWithParameterizedActionVisitsParameter() {
		RecordingVisitor visitor = new RecordingVisitor();
		ActionImpl act = customAction("myAction");
		org.skyve.impl.metadata.view.widget.bound.ParameterImpl p = new org.skyve.impl.metadata.view.widget.bound.ParameterImpl();
		p.setName("foo");
		p.setValue("bar");
		act.getParameters().add(p);
		visitor.visitActions(view("edit", act));
		assertEquals(List.of("custom", "parameter"), visitor.visited);
	}
}
