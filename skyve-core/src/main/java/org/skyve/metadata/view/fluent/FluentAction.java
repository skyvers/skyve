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

abstract class FluentAction<T extends FluentAction<T>> {
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
		if (action instanceof DefaultsAction) {
			return (T) new FluentDefaultsAction().from((DefaultsAction) action);
		}
		if (action instanceof OKAction) {
			return (T) new FluentOKAction().from((OKAction) action);
		}
		if (action instanceof SaveAction) {
			return (T) new FluentSaveAction().from((SaveAction) action);
		}
		if (action instanceof DeleteAction) {
			return (T) new FluentDeleteAction().from((DeleteAction) action);
		}
		if (action instanceof CancelAction) {
			return (T) new FluentCancelAction().from((CancelAction) action);
		}
		if (action instanceof CustomAction) {
			return (T) new FluentCustomAction().from((CustomAction) action);
		}
		if (action instanceof ReportAction) {
			return (T) new FluentReportAction().from((ReportAction) action);
		}
		if (action instanceof ZoomOutAction) {
			return (T) new FluentZoomOutAction().from((ZoomOutAction) action);
		}
		if (action instanceof RemoveAction) {
			return (T) new FluentRemoveAction().from((RemoveAction) action);
		}
		if (action instanceof AddAction) {
			return (T) new FluentAddAction().from((AddAction) action);
		}
		if (action instanceof NewAction) {
			return (T) new FluentNewAction().from((NewAction) action);
		}
		if (action instanceof DownloadAction) {
			return (T) new FluentDownloadAction().from((DownloadAction) action);
		}
		if (action instanceof UploadAction) {
			return (T) new FluentUploadAction().from((UploadAction) action);
		}
		if (action instanceof BizExportAction) {
			return (T) new FluentBizExportAction().from((BizExportAction) action);
		}
		if (action instanceof BizImportAction) {
			return (T) new FluentBizImportAction().from((BizImportAction) action);
		}
		if (action instanceof PrintAction) {
			return (T) new FluentPrintAction().from((PrintAction) action);
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
