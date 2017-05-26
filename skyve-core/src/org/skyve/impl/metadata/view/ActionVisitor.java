package org.skyve.impl.metadata.view;

import java.util.List;

import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;

public abstract class ActionVisitor {
	public final void visitActions(ViewImpl view) {
		ViewType type = view.getType();
		for (org.skyve.metadata.view.Action action : view.getActions()) {
			visit(type, (ActionImpl) action);
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
		List<FilterParameter> parameters = filterable.getParameters();
		if (parameters != null) {
			for (FilterParameter parameter : parameters) {
				visitFilterParameter(parameter, parentVisible, parentEnabled);
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
	public abstract void visitParameter(Parameter parameter,
											boolean parentVisible,
											boolean parentEnabled);
	public abstract void visitFilterParameter(FilterParameter parameter,
												boolean parentVisible,
												boolean parentEnabled);

	private void visit(ViewType viewType, ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewType, implicitName, action);
		}
		else {
			visitCustomAction(action);
		}
	}

	private void visit(ViewType viewType, ImplicitActionName implicitName, ActionImpl action) {
		if (implicitName == ImplicitActionName.DEFAULTS) {
			if (viewType == ViewType.list) {
				visit(viewType, ImplicitActionName.New, action);
			}
			else { // edit view
				for (ImplicitActionName value : ImplicitActionName.values()) {
					// Render implicit actions that appear on edit views
					if ((value != ImplicitActionName.DEFAULTS) && 
							(value != ImplicitActionName.New) &&
							(value != ImplicitActionName.Report) && 
							(value != ImplicitActionName.BizExport) &&
							(value != ImplicitActionName.BizImport) && 
							(value != ImplicitActionName.Download) && 
							(value != ImplicitActionName.Upload) && 
							(value != ImplicitActionName.Navigate)) {
						visit(viewType, value, action);
					}
				}
			}
		}
		else if (implicitName == ImplicitActionName.Add) {
			visitAddAction(action);
		}
		else if (implicitName == ImplicitActionName.Remove) {
			visitRemoveAction(action);
		}
		else if (implicitName == ImplicitActionName.ZoomOut) {
			visitZoomOutAction(action);
		}
		else if (implicitName == ImplicitActionName.Navigate) {
			visitNavigateAction(action);
		}
		else if (implicitName == ImplicitActionName.OK) {
			visitOKAction(action);
		}
		else if (implicitName == ImplicitActionName.Save) {
			visitSaveAction(action);
		}
		else if (implicitName == ImplicitActionName.Cancel) {
			visitCancelAction(action);
		}
		else if (implicitName == ImplicitActionName.Delete) {
			visitDeleteAction(action);
		}
		else if (implicitName == ImplicitActionName.Report) {
			visitReportAction(action);
		}
		else if (implicitName == ImplicitActionName.BizExport) {
			visitBizExportAction(action);
		}
		else if (implicitName == ImplicitActionName.BizImport) {
			visitBizImportAction(action);
		}
		else if (implicitName == ImplicitActionName.Download) {
			visitDownloadAction(action);
		}
		else if (implicitName == ImplicitActionName.Upload) {
			visitUploadAction(action);
		}
		else if (implicitName == ImplicitActionName.New) {
			visitNewAction(action);
		}
		else if (implicitName == ImplicitActionName.Edit) {
			visitEditAction(action);
		}
		else {
			throw new IllegalArgumentException(implicitName + " is not supported by ActionVisitor.");
		}
	}
}
