package org.skyve.wildcat.metadata.view;

import java.util.List;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.bound.Parameter;

public abstract class ActionVisitor {
	public final void visitActions(ViewImpl view) throws MetaDataException {
		ViewType type = view.getType();
		for (org.skyve.metadata.view.Action action : view.getActions()) {
			visit(type, (Action) action);
			visitParameterizable(action, true, true);
		}
	}

	protected void visitParameterizable(Parameterizable parameterizable,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException {
		List<Parameter> parameters = parameterizable.getParameters();
		if (parameters != null) {
			for (Parameter parameter : parameters) {
				visitParameter(parameter, parentVisible, parentEnabled);
			}
		}
	}

	public abstract void visitAction(Action action) throws MetaDataException;
	public abstract void visitAddAction(Action action) throws MetaDataException;
	public abstract void visitRemoveAction(Action action) throws MetaDataException;
	public abstract void visitZoomOutAction(Action action) throws MetaDataException;
	public abstract void visitNavigateAction(Action action) throws MetaDataException;
	public abstract void visitOKAction(Action action) throws MetaDataException;
	public abstract void visitSaveAction(Action action) throws MetaDataException;
	public abstract void visitCancelAction(Action action) throws MetaDataException;
	public abstract void visitDeleteAction(Action action) throws MetaDataException;
	public abstract void visitReportAction(Action action) throws MetaDataException;
	public abstract void visitBizExportAction(Action action) throws MetaDataException;
	public abstract void visitBizImportAction(Action action) throws MetaDataException;
	public abstract void visitUploadAction(Action action) throws MetaDataException;
	public abstract void visitNewAction(Action action) throws MetaDataException;
	public abstract void visitEditAction(Action action) throws MetaDataException;
	public abstract void visitParameter(Parameter parameter,
											boolean parentVisible,
											boolean parentEnabled)
	throws MetaDataException;

	private void visit(ViewType viewType, Action action) 
	throws MetaDataException {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewType, implicitName, action);
		}
		else {
			visitAction(action);
		}
	}

	private void visit(ViewType viewType, ImplicitActionName implicitName, Action action) 
	throws MetaDataException {
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
