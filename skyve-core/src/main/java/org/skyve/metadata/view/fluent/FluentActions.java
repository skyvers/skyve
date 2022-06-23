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

public class FluentActions {
	private Actions actions = null;
	
	public FluentActions() {
		actions = new Actions();
	}
	
	public FluentActions(Actions actions) {
		this.actions = actions;
	}
	
	public FluentActions from(String widgetId, @SuppressWarnings("hiding") Collection<Action> actions) {
		widgetId(widgetId);
		actions.forEach(a -> addAction(FluentAction.from(a)));
		return this;
	}
	
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
		return actions.getActions().stream().filter(a -> ((a instanceof ClassAction) && className.equals(((ClassAction) a).getClassName()))).findAny().orElse(null);
	}

	public FluentActions addAddAction(FluentAddAction action) {
		return addAction(action);
	}

	public FluentActions addAddAction(int index, FluentAddAction action) {
		return addAction(index, action);
	}

	public FluentAddAction findNamedAddAction(String name) {
		AddAction result = (AddAction) findNamedAction(name);
		if (result != null) {
			return new FluentAddAction(result);
		}
		return null;
	}

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

	public FluentCancelAction findNamedCancelAction(String name) {
		CancelAction result = (CancelAction) findNamedAction(name);
		if (result != null) {
			return new FluentCancelAction(result);
		}
		return null;
	}

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

	public FluentDefaultsAction findNamedDefaultsAction(String name) {
		DefaultsAction result = (DefaultsAction) findNamedAction(name);
		if (result != null) {
			return new FluentDefaultsAction(result);
		}
		return null;
	}

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

	public FluentNewAction findNamedNewAction(String name) {
		NewAction result = (NewAction) findNamedAction(name);
		if (result != null) {
			return new FluentNewAction(result);
		}
		return null;
	}

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

	public FluentRemoveAction findNamedRemoveAction(String name) {
		RemoveAction result = (RemoveAction) findNamedAction(name);
		if (result != null) {
			return new FluentRemoveAction(result);
		}
		return null;
	}

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

	public FluentDeleteAction findNamedDeleteAction(String name) {
		DeleteAction result = (DeleteAction) findNamedAction(name);
		if (result != null) {
			return new FluentDeleteAction(result);
		}
		return null;
	}

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

	public FluentOKAction findNamedOKAction(String name) {
		OKAction result = (OKAction) findNamedAction(name);
		if (result != null) {
			return new FluentOKAction(result);
		}
		return null;
	}

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

	public FluentPrintAction findNamedPrintAction(String name) {
		PrintAction result = (PrintAction) findNamedAction(name);
		if (result != null) {
			return new FluentPrintAction(result);
		}
		return null;
	}

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

	public FluentSaveAction findNamedSaveAction(String name) {
		SaveAction result = (SaveAction) findNamedAction(name);
		if (result != null) {
			return new FluentSaveAction(result);
		}
		return null;
	}

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

	public FluentZoomOutAction findNamedZoomOutAction(String name) {
		ZoomOutAction result = (ZoomOutAction) findNamedAction(name);
		if (result != null) {
			return new FluentZoomOutAction(result);
		}
		return null;
	}

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

	public FluentBizExportAction findNamedBizExportAction(String name) {
		BizExportAction result = (BizExportAction) findNamedAction(name);
		if (result != null) {
			return new FluentBizExportAction(result);
		}
		return null;
	}

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

	public FluentBizImportAction findNamedBizImportAction(String name) {
		BizImportAction result = (BizImportAction) findNamedAction(name);
		if (result != null) {
			return new FluentBizImportAction(result);
		}
		return null;
	}

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

	public FluentCustomAction findNamedCustomAction(String name) {
		CustomAction result = (CustomAction) findNamedAction(name);
		if (result != null) {
			return new FluentCustomAction(result);
		}
		return null;
	}

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
	
	public FluentDownloadAction findNamedDownloadAction(String name) {
		DownloadAction result = (DownloadAction) findNamedAction(name);
		if (result != null) {
			return new FluentDownloadAction(result);
		}
		return null;
	}

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

	public FluentUploadAction findNamedUploadAction(String name) {
		UploadAction result = (UploadAction) findNamedAction(name);
		if (result != null) {
			return new FluentUploadAction(result);
		}
		return null;
	}

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

	public FluentReportAction findNamedReportAction(String name) {
		ReportAction result = (ReportAction) findNamedAction(name);
		if (result != null) {
			return new FluentReportAction(result);
		}
		return null;
	}

	public FluentReportAction findReportAction(String reportName) {
		ReportAction result = (ReportAction) actions.getActions().stream().filter(a -> ((a instanceof ReportAction) && reportName.equals(((ReportAction) a).getReportName()))).findAny().orElse(null);
		if (result != null) {
			return new FluentReportAction(result);
		}
		return null;
	}

	public FluentActions removeReportAction(String reportName) {
		actions.getActions().removeIf(a -> ((a instanceof ReportAction) && reportName.equals(((ReportAction) a).getReportName())));
		return this;
	}

	public FluentActions removeNamedAction(String name) {
		actions.getActions().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	public FluentActions removeImplicitAction(ImplicitActionName name) {
		actions.getActions().removeIf(a -> name.equals(a.getImplicitName()));
		return this;
	}

	public FluentActions removeClassAction(String className) {
		actions.getActions().removeIf(a -> ((a instanceof ClassAction) && className.equals(((ClassAction) a).getClassName())));
		return this;
	}

	public FluentActions removeAction(int index) {
		actions.getActions().remove(index);
		return this;
	}

	public FluentActions clearActions() {
		actions.getActions().clear();
		return this;
	}

	public Actions get() {
		return actions;
	}
}
