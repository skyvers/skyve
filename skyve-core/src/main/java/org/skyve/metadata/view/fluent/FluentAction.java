package org.skyve.metadata.view.fluent;

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
import org.skyve.metadata.view.Action;

public abstract class FluentAction<T extends FluentAction<T>> {
	protected FluentAction() {
		// nothing to see
	}

	@SuppressWarnings("unchecked")
	protected T fromBase(ActionMetaData action) {
		name(action.getName());
		displayName(action.getDisplayName());
		toolTip(action.getToolTip());
		relativeIconFileName(action.getRelativeIconFileName());
		iconStyleClass(action.getIconStyleClass());
		confirmationText(action.getConfirmationText());
		disabledConditionName(action.getDisabledConditionName());
		invisibleConditionName(action.getInvisibleConditionName());
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return (T) this;
	}

	public static <T extends FluentAction<T>> T from(Action action) {
		ActionMetaData a = ((ActionImpl) action).toRepositoryAction();
		return from(a);
	}

	@SuppressWarnings("unchecked")
	public static <T extends FluentAction<T>> T from(ActionMetaData action) {
		if (action instanceof DefaultsAction defaultsAction) {
			return (T) new FluentDefaultsAction().from(defaultsAction);
		}
		if (action instanceof OKAction oKAction) {
			return (T) new FluentOKAction().from(oKAction);
		}
		if (action instanceof SaveAction saveAction) {
			return (T) new FluentSaveAction().from(saveAction);
		}
		if (action instanceof DeleteAction deleteAction) {
			return (T) new FluentDeleteAction().from(deleteAction);
		}
		if (action instanceof CancelAction cancelAction) {
			return (T) new FluentCancelAction().from(cancelAction);
		}
		if (action instanceof CustomAction customAction) {
			return (T) new FluentCustomAction().from(customAction);
		}
		if (action instanceof ReportAction reportAction) {
			return (T) new FluentReportAction().from(reportAction);
		}
		if (action instanceof ZoomOutAction zoomOutAction) {
			return (T) new FluentZoomOutAction().from(zoomOutAction);
		}
		if (action instanceof RemoveAction removeAction) {
			return (T) new FluentRemoveAction().from(removeAction);
		}
		if (action instanceof AddAction addAction) {
			return (T) new FluentAddAction().from(addAction);
		}
		if (action instanceof NewAction newAction) {
			return (T) new FluentNewAction().from(newAction);
		}
		if (action instanceof DownloadAction downloadAction) {
			return (T) new FluentDownloadAction().from(downloadAction);
		}
		if (action instanceof UploadAction uploadAction) {
			return (T) new FluentUploadAction().from(uploadAction);
		}
		if (action instanceof BizExportAction bizExportAction) {
			return (T) new FluentBizExportAction().from(bizExportAction);
		}
		if (action instanceof BizImportAction bizImportAction) {
			return (T) new FluentBizImportAction().from(bizImportAction);
		}
		if (action instanceof PrintAction printAction) {
			return (T) new FluentPrintAction().from(printAction);
		}
		throw new IllegalStateException(action + " not catered for");
	}

	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T toolTip(String toolTip) {
		get().setToolTip(toolTip);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T relativeIconFileName(String relativeIconFileName) {
		get().setRelativeIconFileName(relativeIconFileName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T iconStyleClass(String iconStyleClass) {
		get().setIconStyleClass(iconStyleClass);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T confirmationText(String confirmationText) {
		get().setConfirmationText(confirmationText);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T disabledConditionName(String disabledConditionName) {
		get().setDisabledConditionName(disabledConditionName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T invisibleConditionName(String invisibleConditionName) {
		get().setInvisibleConditionName(invisibleConditionName);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T putProperty(String k, String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	public abstract ActionMetaData get();
}
