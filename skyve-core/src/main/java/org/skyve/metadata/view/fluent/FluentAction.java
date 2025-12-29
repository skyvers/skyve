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
		if (action instanceof DefaultsAction defaults) {
			return (T) new FluentDefaultsAction().from(defaults);
		}
		if (action instanceof OKAction ok) {
			return (T) new FluentOKAction().from(ok);
		}
		if (action instanceof SaveAction save) {
			return (T) new FluentSaveAction().from(save);
		}
		if (action instanceof DeleteAction delete) {
			return (T) new FluentDeleteAction().from(delete);
		}
		if (action instanceof CancelAction cancel) {
			return (T) new FluentCancelAction().from(cancel);
		}
		if (action instanceof CustomAction custom) {
			return (T) new FluentCustomAction().from(custom);
		}
		if (action instanceof ReportAction report) {
			return (T) new FluentReportAction().from(report);
		}
		if (action instanceof ZoomOutAction zoom) {
			return (T) new FluentZoomOutAction().from(zoom);
		}
		if (action instanceof RemoveAction remove) {
			return (T) new FluentRemoveAction().from(remove);
		}
		if (action instanceof AddAction add) {
			return (T) new FluentAddAction().from(add);
		}
		if (action instanceof NewAction newAction) {
			return (T) new FluentNewAction().from(newAction);
		}
		if (action instanceof DownloadAction download) {
			return (T) new FluentDownloadAction().from(download);
		}
		if (action instanceof UploadAction upload) {
			return (T) new FluentUploadAction().from(upload);
		}
		if (action instanceof BizExportAction export) {
			return (T) new FluentBizExportAction().from(export);
		}
		if (action instanceof BizImportAction bizImport) {
			return (T) new FluentBizImportAction().from(bizImport);
		}
		if (action instanceof PrintAction print) {
			return (T) new FluentPrintAction().from(print);
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
