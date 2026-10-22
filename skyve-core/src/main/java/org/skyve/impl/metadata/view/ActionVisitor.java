package org.skyve.impl.metadata.view;

import java.util.List;

import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Filterable;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;

/**
 * Abstract visitor that traverses the action list of a {@link ViewImpl}.
 *
 * <p>Dispatch preserves view declaration order. Custom actions are sent to
 * {@link #visitCustomAction(ActionImpl)}; implicit actions are sent to their
 * type-specific callbacks. The {@link ImplicitActionName#DEFAULTS} placeholder
 * expands to the implicit actions supported by the declaring list or edit view.
 * Parameters are visited after their owning action.
 *
 * <p>Subclasses implement callbacks to inspect, validate, or render actions.
 * {@link ViewVisitor} extends this class to combine action and widget traversal.
 *
 * <p>Threading: not thread-safe; one instance per traversal.
 *
 * @see ViewVisitor
 * @see NoOpViewVisitor
 */
public abstract class ActionVisitor {
	/**
	 * Creates an action visitor for use by a concrete traversal implementation.
	 */
	protected ActionVisitor() {
		// Subclasses supply all traversal context.
	}

	/**
	 * Visits each action declared by the view in declaration order and then visits its parameters.
	 *
	 * @param view the view metadata to traverse; must not be null
	 */
	public final void visitActions(@Nonnull ViewImpl view) {
		String name = view.getName();
		for (org.skyve.metadata.view.Action action : view.getActions()) {
			visit(name, (ActionImpl) action);
			visitParameterizable(action, true, true);
		}
	}

	/**
	 * Visits the parameters exposed by parameterisable metadata in declaration order.
	 *
	 * @param parameteriable the parameterisable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	protected void visitParameterizable(@Nonnull Parameterizable parameterizable,
											boolean parentVisible,
											boolean parentEnabled) {
		List<Parameter> parameters = parameterizable.getParameters();
		for (Parameter parameter : parameters) {
			visitParameter(parameter, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits filter and ordinary parameters exposed by filterable metadata in declaration order.
	 *
	 * @param filterable the filterable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	protected void visitFilterable(@Nonnull Filterable filterable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<FilterParameter> filterParameters = filterable.getFilterParameters();
		for (FilterParameter parameter : filterParameters) {
			visitFilterParameter(parameter, parentVisible, parentEnabled);
		}
		List<Parameter> parameters = filterable.getParameters();
		for (Parameter parameter : parameters) {
			visitParameter(parameter, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits an explicitly named custom action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitCustomAction(@Nonnull ActionImpl action);

	/**
	 * Visits the add implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitAddAction(@Nonnull ActionImpl action);

	/**
	 * Visits the remove implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitRemoveAction(@Nonnull ActionImpl action);

	/**
	 * Visits the zoom out implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitZoomOutAction(@Nonnull ActionImpl action);

	/**
	 * Visits the navigate implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitNavigateAction(@Nonnull ActionImpl action);

	/**
	 * Visits the OK implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitOKAction(@Nonnull ActionImpl action);

	/**
	 * Visits the save implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitSaveAction(@Nonnull ActionImpl action);

	/**
	 * Visits the cancel implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitCancelAction(@Nonnull ActionImpl action);

	/**
	 * Visits the delete implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitDeleteAction(@Nonnull ActionImpl action);

	/**
	 * Visits the report implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitReportAction(@Nonnull ActionImpl action);

	/**
	 * Visits the business-export implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitBizExportAction(@Nonnull ActionImpl action);

	/**
	 * Visits the business-import implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitBizImportAction(@Nonnull ActionImpl action);

	/**
	 * Visits the download implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitDownloadAction(@Nonnull ActionImpl action);

	/**
	 * Visits the upload implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitUploadAction(@Nonnull ActionImpl action);

	/**
	 * Visits the new implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitNewAction(@Nonnull ActionImpl action);

	/**
	 * Visits the edit implicit action.
	 *
	 * @param action the action metadata; must not be null
	 */
	public abstract void visitEditAction(@Nonnull ActionImpl action);

	/**
	 * Visits the print implicit action.
	 *
	 * <p>The base implementation is intentionally empty for backward compatibility;
	 * subclasses override it when their output target supports printing.
	 *
	 * @param action the action metadata; must not be null
	 */
	public void visitPrintAction(@Nonnull ActionImpl action) {
		// nothing to see here
	}
	/**
	 * Visits one ordinary action or widget parameter.
	 *
	 * @param parameter the parameter metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitParameter(@Nonnull Parameter parameter,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits one filter parameter.
	 *
	 * @param parameter the parameter metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitFilterParameter(@Nonnull FilterParameter parameter,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Resolves and dispatches one declared action to the appropriate callback.
	 *
	 * @param viewName the name of the view declaring the action; must not be null
	 * @param action the action metadata; must not be null
	 */
	private void visit(@Nonnull String viewName, @Nonnull ActionImpl action) {
		ImplicitActionName implicitName = action.getImplicitName();
		if (implicitName != null) {
			visit(viewName, implicitName, action);
		}
		else {
			visitCustomAction(action);
		}
	}

	/**
	 * Dispatches one resolved implicit action to its type-specific callback.
	 *
	 * @param viewName the name of the view declaring the action; must not be null
	 * @param implicitName the resolved implicit action name; must not be null
	 * @param action the action metadata; must not be null
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visit(@Nonnull String viewName, @Nonnull ImplicitActionName implicitName, @Nonnull ActionImpl action) {
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
