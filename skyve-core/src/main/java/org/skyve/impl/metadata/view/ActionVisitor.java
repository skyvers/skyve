package org.skyve.impl.metadata.view;

import java.util.List;

import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

public abstract class ActionVisitor {
	public final void visitActions(ViewImpl view) {
		String name = view.getName();
		for (org.skyve.metadata.view.Action action : view.getActions()) {
			visit(name, (ActionImpl) action);
			visitParameterizable(action, true, true);
		}
	}

	protected void visitParameterizable(Parameterizable parameterizable,
											boolean parentVisible,
											boolean parentEnabled) {
		List<Parameter> parameters = parameterizable.getParameters();
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				visitParameter(parameter, parentVisible, parentEnabled);
			}
		}
	}

	protected void visitFilterable(Filterable filterable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<FilterParameter> filterParameters = filterable.getFilterParameters();
		if (filterParameters != null) {
			for (FilterParameter parameter : filterParameters) {
				visitFilterParameter(parameter, parentVisible, parentEnabled);
			}
		}
		List<Parameter> parameters = filterable.getParameters();
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				visitParameter(parameter, parentVisible, parentEnabled);
			}
		}
	}

	public abstract void visitCustomAction(ActionImpl action);
	public abstract void visitAddAction(ActionImpl action);
	public abstract void visitRemoveAction(ActionImpl action);
	public abstract void visitZoomOutAction(ActionImpl action);
	public abstract void visitNavigateAction(ActionImpl action);
	public abstract void visitOKAction(ActionImpl action);
	public abstract void visitSaveAction(ActionImpl action);
	public abstract void visitCancelAction(ActionImpl action);
	public abstract void visitDeleteAction(ActionImpl action);
	public abstract void visitReportAction(ActionImpl action);
	public abstract void visitBizExportAction(ActionImpl action);
	public abstract void visitBizImportAction(ActionImpl action);
	public abstract void visitDownloadAction(ActionImpl action);
	public abstract void visitUploadAction(ActionImpl action);
	public abstract void visitNewAction(ActionImpl action);
	public abstract void visitEditAction(ActionImpl action);
	public void visitPrintAction(@SuppressWarnings("unused") ActionImpl action) {
		// nothing to see here
	}
	public abstract void visitParameter(Parameter parameter,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitFilterParameter(FilterParameter parameter,
												boolean parentVisible,
												boolean parentEnabled);

	private void visit(String viewName, ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewName, implicitName, action);
		}
		else {
			visitCustomAction(action);
		}
	}

	private void visit(String viewName, ImplicitActionName implicitName, ActionImpl action) {
		if (ImplicitActionName.DEFAULTS.equals(implicitName)) {
			if (ViewType.list.toString().equals(viewName)) {
				visit(viewName, ImplicitActionName.New, action);
			}
			else { // edit view
				for (ImplicitActionName value : ImplicitActionName.values()) {
					// Render implicit actions that appear on edit views
					if ((! ImplicitActionName.DEFAULTS.equals(value)) && 
							(! ImplicitActionName.New.equals(value)) &&
							(! ImplicitActionName.Report.equals(value)) &&
							(! ImplicitActionName.BizExport.equals(value)) &&
							(! ImplicitActionName.BizImport.equals(value)) &&
							(! ImplicitActionName.Download.equals(value)) &&
							(! ImplicitActionName.Upload.equals(value)) &&
							(! ImplicitActionName.Navigate.equals(value)) &&
							(! ImplicitActionName.Print.equals(value))) {
						visit(viewName, value, action);
					}
				}
			}
		}
		else if (ImplicitActionName.Add.equals(implicitName)) {
			visitAddAction(action);
		}
		else if (ImplicitActionName.Remove.equals(implicitName)) {
			visitRemoveAction(action);
		}
		else if (ImplicitActionName.ZoomOut.equals(implicitName)) {
			visitZoomOutAction(action);
		}
		else if (ImplicitActionName.Navigate.equals(implicitName)) {
			visitNavigateAction(action);
		}
		else if (ImplicitActionName.OK.equals(implicitName)) {
			visitOKAction(action);
		}
		else if (ImplicitActionName.Save.equals(implicitName)) {
			visitSaveAction(action);
		}
		else if (ImplicitActionName.Cancel.equals(implicitName)) {
			visitCancelAction(action);
		}
		else if (ImplicitActionName.Delete.equals(implicitName)) {
			visitDeleteAction(action);
		}
		else if (ImplicitActionName.Report.equals(implicitName)) {
			visitReportAction(action);
		}
		else if (ImplicitActionName.BizExport.equals(implicitName)) {
			visitBizExportAction(action);
		}
		else if (ImplicitActionName.BizImport.equals(implicitName)) {
			visitBizImportAction(action);
		}
		else if (ImplicitActionName.Download.equals(implicitName)) {
			visitDownloadAction(action);
		}
		else if (ImplicitActionName.Upload.equals(implicitName)) {
			visitUploadAction(action);
		}
		else if (ImplicitActionName.New.equals(implicitName)) {
			visitNewAction(action);
		}
		else if (ImplicitActionName.Edit.equals(implicitName)) {
			visitEditAction(action);
		}
		else if (ImplicitActionName.Print.equals(implicitName)) {
			visitPrintAction(action);
		}
		else {
			throw new IllegalArgumentException(implicitName + " is not supported by ActionVisitor.");
		}
	}
}
