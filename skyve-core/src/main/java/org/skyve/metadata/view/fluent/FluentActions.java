package org.skyve.metadata.view.fluent;

import java.util.Collection;

import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.ClassAction;
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
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;

/**
 * Builds and queries an {@link Actions} collection using fluent wrappers.
 *
 * <p>Side effects: mutates the wrapped {@link Actions} instance by appending,
 * replacing, or removing action metadata entries.
 */
public class FluentActions {
	private Actions actions = null;
	
	/**
	 * Creates a fluent action collection backed by a new {@link Actions} instance.
	 */
	public FluentActions() {
		actions = new Actions();
	}
	
	/**
	 * Creates a fluent action collection backed by the supplied instance.
	 *
	 * @param actions
	 *            the mutable action collection to wrap
	 */
	public FluentActions(Actions actions) {
		this.actions = actions;
	}
	
	/**
	 * Replaces this collection's widget id and appends converted actions.
	 *
	 * <p>Side effects: updates {@code widgetId} and appends each converted action to the
	 * wrapped collection in encounter order.
	 *
	 * @param widgetId
	 *            the owning widget identifier
	 * @param actions
	 *            actions to convert and append
	 * @return this builder
	 */
	public FluentActions from(String widgetId, @SuppressWarnings("hiding") Collection<Action> actions) {
		widgetId(widgetId);
		actions.forEach(a -> addAction(FluentAction.from(a)));
		return this;
	}
	
	/**
	 * Sets the owning widget identifier for the wrapped collection.
	 *
	 * @param widgetId
	 *            the owning widget identifier
	 * @return this builder
	 */
	public FluentActions widgetId(String widgetId) {
		actions.setWidgetId(widgetId);
		return this;
	}
	
	private FluentActions addAction(FluentAction<?> action) {
		actions.getActions().add(action.get());
		return this;
	}
	
	private FluentActions addAction(int index, FluentAction<?> action) {
		actions.getActions().add(index, action.get());
		return this;
	}

	private ActionMetaData findNamedAction(String name) {
		return actions.getActions().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}

	private ActionMetaData findImplicitAction(ImplicitActionName name) {
		return actions.getActions().stream().filter(a -> name.equals(a.getImplicitName())).findAny().orElse(null);
	}

	private ActionMetaData findClassAction(String className) {
		return actions.getActions().stream().filter(a -> ((a instanceof ClassAction ca) && className.equals(ca.getClassName()))).findAny().orElse(null);
	}

	public FluentActions addAddAction(FluentAddAction action) {
		return addAction(action);
	}

	public FluentActions addAddAction(int index, FluentAddAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds an add action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentAddAction findNamedAddAction(String name) {
		AddAction result = (AddAction) findNamedAction(name);
		if (result != null) {
			return new FluentAddAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit add action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit add action exists
	 */
	public FluentAddAction findAddAction() {
		AddAction result = (AddAction) findImplicitAction(ImplicitActionName.Add);
		if (result != null) {
			return new FluentAddAction(result);
		}
		return null;
	}
	
	public FluentActions addCancelAction(FluentCancelAction action) {
		return addAction(action);
	}

	public FluentActions addCancelAction(int index, FluentCancelAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a cancel action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentCancelAction findNamedCancelAction(String name) {
		CancelAction result = (CancelAction) findNamedAction(name);
		if (result != null) {
			return new FluentCancelAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit cancel action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit cancel action exists
	 */
	public FluentCancelAction findCancelAction() {
		CancelAction result = (CancelAction) findImplicitAction(ImplicitActionName.Cancel);
		if (result != null) {
			return new FluentCancelAction(result);
		}
		return null;
	}
	
	public FluentActions addDefaultsAction(FluentDefaultsAction action) {
		return addAction(action);
	}
	
	public FluentActions addDefaultsAction(int index, FluentDefaultsAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a defaults action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentDefaultsAction findNamedDefaultsAction(String name) {
		DefaultsAction result = (DefaultsAction) findNamedAction(name);
		if (result != null) {
			return new FluentDefaultsAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit defaults action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit defaults action exists
	 */
	public FluentDefaultsAction findDefaultsAction() {
		DefaultsAction result = (DefaultsAction) findImplicitAction(ImplicitActionName.DEFAULTS);
		if (result != null) {
			return new FluentDefaultsAction(result);
		}
		return null;
	}

	public FluentActions addNewAction(FluentNewAction action) {
		return addAction(action);
	}

	public FluentActions addNewAction(int index, FluentNewAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a new action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentNewAction findNamedNewAction(String name) {
		NewAction result = (NewAction) findNamedAction(name);
		if (result != null) {
			return new FluentNewAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit new action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit new action exists
	 */
	public FluentNewAction findNewAction() {
		NewAction result = (NewAction) findImplicitAction(ImplicitActionName.New);
		if (result != null) {
			return new FluentNewAction(result);
		}
		return null;
	}

	public FluentActions addRemoveAction(FluentRemoveAction action) {
		return addAction(action);
	}

	public FluentActions addRemoveAction(int index, FluentRemoveAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a remove action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentRemoveAction findNamedRemoveAction(String name) {
		RemoveAction result = (RemoveAction) findNamedAction(name);
		if (result != null) {
			return new FluentRemoveAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit remove action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit remove action exists
	 */
	public FluentRemoveAction findRemoveAction() {
		RemoveAction result = (RemoveAction) findImplicitAction(ImplicitActionName.Remove);
		if (result != null) {
			return new FluentRemoveAction(result);
		}
		return null;
	}

	public FluentActions addDeleteAction(FluentDeleteAction action) {
		return addAction(action);
	}
	
	public FluentActions addDeleteAction(int index, FluentDeleteAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a delete action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentDeleteAction findNamedDeleteAction(String name) {
		DeleteAction result = (DeleteAction) findNamedAction(name);
		if (result != null) {
			return new FluentDeleteAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit delete action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit delete action exists
	 */
	public FluentDeleteAction findDeleteAction() {
		DeleteAction result = (DeleteAction) findImplicitAction(ImplicitActionName.Delete);
		if (result != null) {
			return new FluentDeleteAction(result);
		}
		return null;
	}

	public FluentActions addOKAction(FluentOKAction action) {
		return addAction(action);
	}

	public FluentActions addOKAction(int index, FluentOKAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds an OK action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentOKAction findNamedOKAction(String name) {
		OKAction result = (OKAction) findNamedAction(name);
		if (result != null) {
			return new FluentOKAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit OK action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit OK action exists
	 */
	public FluentOKAction findOKAction() {
		OKAction result = (OKAction) findImplicitAction(ImplicitActionName.OK);
		if (result != null) {
			return new FluentOKAction(result);
		}
		return null;
	}

	public FluentActions addPrintAction(FluentPrintAction action) {
		return addAction(action);
	}

	public FluentActions addPrintAction(int index, FluentPrintAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a print action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentPrintAction findNamedPrintAction(String name) {
		PrintAction result = (PrintAction) findNamedAction(name);
		if (result != null) {
			return new FluentPrintAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit print action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit print action exists
	 */
	public FluentPrintAction findPrintAction() {
		PrintAction result = (PrintAction) findImplicitAction(ImplicitActionName.Print);
		if (result != null) {
			return new FluentPrintAction(result);
		}
		return null;
	}

	public FluentActions addSaveAction(FluentSaveAction action) {
		return addAction(action);
	}

	public FluentActions addSaveAction(int index, FluentSaveAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a save action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentSaveAction findNamedSaveAction(String name) {
		SaveAction result = (SaveAction) findNamedAction(name);
		if (result != null) {
			return new FluentSaveAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit save action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit save action exists
	 */
	public FluentSaveAction findSaveAction() {
		SaveAction result = (SaveAction) findImplicitAction(ImplicitActionName.Save);
		if (result != null) {
			return new FluentSaveAction(result);
		}
		return null;
	}

	public FluentActions addZoomOutAction(FluentZoomOutAction action) {
		return addAction(action);
	}

	public FluentActions addZoomOutAction(int index, FluentZoomOutAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a zoom-out action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentZoomOutAction findNamedZoomOutAction(String name) {
		ZoomOutAction result = (ZoomOutAction) findNamedAction(name);
		if (result != null) {
			return new FluentZoomOutAction(result);
		}
		return null;
	}

	/**
	 * Finds the implicit zoom-out action.
	 *
	 * @return a fluent wrapper, or {@code null} when no implicit zoom-out action exists
	 */
	public FluentZoomOutAction findZoomOutAction() {
		ZoomOutAction result = (ZoomOutAction) findImplicitAction(ImplicitActionName.Save);
		if (result != null) {
			return new FluentZoomOutAction(result);
		}
		return null;
	}

	public FluentActions addBizExportAction(FluentBizExportAction action) {
		return addAction(action);
	}

	public FluentActions addBizExportAction(int index, FluentBizExportAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a business export action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentBizExportAction findNamedBizExportAction(String name) {
		BizExportAction result = (BizExportAction) findNamedAction(name);
		if (result != null) {
			return new FluentBizExportAction(result);
		}
		return null;
	}

	/**
	 * Finds a business export action by configured class name.
	 *
	 * @param className
	 *            the class name configured on class-based actions
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentBizExportAction findBizExportAction(String className) {
		BizExportAction result = (BizExportAction) findClassAction(className);
		if (result != null) {
			return new FluentBizExportAction(result);
		}
		return null;
	}
	
	public FluentActions addBizImportAction(FluentBizImportAction action) {
		return addAction(action);
	}

	public FluentActions addBizImportAction(int index, FluentBizImportAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a business import action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentBizImportAction findNamedBizImportAction(String name) {
		BizImportAction result = (BizImportAction) findNamedAction(name);
		if (result != null) {
			return new FluentBizImportAction(result);
		}
		return null;
	}

	/**
	 * Finds a business import action by configured class name.
	 *
	 * @param className
	 *            the class name configured on class-based actions
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentBizImportAction findBizImportAction(String className) {
		BizImportAction result = (BizImportAction) findClassAction(className);
		if (result != null) {
			return new FluentBizImportAction(result);
		}
		return null;
	}

	public FluentActions addCustomAction(FluentCustomAction action) {
		return addAction(action);
	}
	
	public FluentActions addCustomAction(int index, FluentCustomAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a custom action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentCustomAction findNamedCustomAction(String name) {
		CustomAction result = (CustomAction) findNamedAction(name);
		if (result != null) {
			return new FluentCustomAction(result);
		}
		return null;
	}

	/**
	 * Finds a custom action by configured class name.
	 *
	 * @param className
	 *            the class name configured on class-based actions
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentCustomAction findCustomAction(String className) {
		CustomAction result = (CustomAction) findClassAction(className);
		if (result != null) {
			return new FluentCustomAction(result);
		}
		return null;
	}

	public FluentActions addDownloadAction(FluentDownloadAction action) {
		return addAction(action);
	}
	
	public FluentActions addDownloadAction(int index, FluentDownloadAction action) {
		return addAction(index, action);
	}
	
	/**
	 * Finds a download action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentDownloadAction findNamedDownloadAction(String name) {
		DownloadAction result = (DownloadAction) findNamedAction(name);
		if (result != null) {
			return new FluentDownloadAction(result);
		}
		return null;
	}

	/**
	 * Finds a download action by configured class name.
	 *
	 * @param className
	 *            the class name configured on class-based actions
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentDownloadAction findDownloadAction(String className) {
		DownloadAction result = (DownloadAction) findClassAction(className);
		if (result != null) {
			return new FluentDownloadAction(result);
		}
		return null;
	}

	public FluentActions addUploadAction(FluentUploadAction action) {
		return addAction(action);
	}
	
	public FluentActions addUploadAction(int index, FluentUploadAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds an upload action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentUploadAction findNamedUploadAction(String name) {
		UploadAction result = (UploadAction) findNamedAction(name);
		if (result != null) {
			return new FluentUploadAction(result);
		}
		return null;
	}

	/**
	 * Finds an upload action by configured class name.
	 *
	 * @param className
	 *            the class name configured on class-based actions
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentUploadAction findUploadAction(String className) {
		UploadAction result = (UploadAction) findClassAction(className);
		if (result != null) {
			return new FluentUploadAction(result);
		}
		return null;
	}

	public FluentActions addReportAction(FluentReportAction action) {
		return addAction(action);
	}
	
	public FluentActions addReportAction(int index, FluentReportAction action) {
		return addAction(index, action);
	}

	/**
	 * Finds a report action by explicit metadata name.
	 *
	 * @param name
	 *            the action metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentReportAction findNamedReportAction(String name) {
		ReportAction result = (ReportAction) findNamedAction(name);
		if (result != null) {
			return new FluentReportAction(result);
		}
		return null;
	}

	/**
	 * Finds a report action by report metadata name.
	 *
	 * @param reportName
	 *            the report metadata name
	 * @return a fluent wrapper, or {@code null} when no match exists
	 */
	public FluentReportAction findReportAction(String reportName) {
		ReportAction result = (ReportAction) actions.getActions().stream().filter(a -> ((a instanceof ReportAction ra) && reportName.equals(ra.getReportName()))).findAny().orElse(null);
		if (result != null) {
			return new FluentReportAction(result);
		}
		return null;
	}

	/**
	 * Removes report actions whose report metadata name matches {@code reportName}.
	 *
	 * <p>Side effects: mutates the wrapped action collection.
	 *
	 * @param reportName
	 *            the report metadata name to remove
	 * @return this builder
	 */
	public FluentActions removeReportAction(String reportName) {
		actions.getActions().removeIf(a -> ((a instanceof ReportAction ra) && reportName.equals(ra.getReportName())));
		return this;
	}

	/**
	 * Removes actions whose explicit metadata name matches {@code name}.
	 *
	 * @param name
	 *            the action metadata name to remove
	 * @return this builder
	 */
	public FluentActions removeNamedAction(String name) {
		actions.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	/**
	 * Removes actions whose implicit action type matches {@code name}.
	 *
	 * @param name
	 *            the implicit action type to remove
	 * @return this builder
	 */
	public FluentActions removeImplicitAction(ImplicitActionName name) {
		actions.getActions().removeIf(a -> name.equals(a.getImplicitName()));
		return this;
	}

	/**
	 * Removes class-based actions whose configured class name matches {@code className}.
	 *
	 * @param className
	 *            the class name to remove
	 * @return this builder
	 */
	public FluentActions removeClassAction(String className) {
		actions.getActions().removeIf(a -> ((a instanceof ClassAction ca) && className.equals(ca.getClassName())));
		return this;
	}

	/**
	 * Removes the action entry at {@code index}.
	 *
	 * @param index
	 *            zero-based index into the action list
	 * @return this builder
	 */
	public FluentActions removeAction(int index) {
		actions.getActions().remove(index);
		return this;
	}

	/**
	 * Removes all actions from the wrapped collection.
	 *
	 * @return this builder
	 */
	public FluentActions clearActions() {
		actions.getActions().clear();
		return this;
	}

	/**
	 * Returns the wrapped mutable action collection.
	 */
	public Actions get() {
		return actions;
	}
}
