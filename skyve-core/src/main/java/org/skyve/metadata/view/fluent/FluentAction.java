package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides fluent configuration for common {@link Action} metadata attributes.
 *
 * @param <T>
 *            the concrete fluent action type
 */
public abstract class FluentAction<T extends FluentAction<T>> {
	/**
	 * Creates a new fluent action base.
	 */
	protected FluentAction() {
		// nothing to see
	}

	/**
	 * Copies shared action metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	protected T fromBase(ActionMetaData action) {
		name(action.getName());
		displayName(action.getDisplayName());
		escapeDisplayName(action.getEscapeDisplayName());
		toolTip(action.getToolTip());
		escapeToolTip(action.getEscapeToolTip());
		relativeIconFileName(action.getRelativeIconFileName());
		iconStyleClass(action.getIconStyleClass());
		confirmationText(action.getConfirmationText());
		escapeConfirm(action.getEscapeConfirm());
		BindUtil.copyAuthoredCondition(action.getXmlDisabledCondition(), action.getXmlEnabledCondition(), this::disabledCondition, this::enabledCondition);
		BindUtil.copyAuthoredCondition(action.getXmlInvisibleCondition(), action.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);
		action.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		return (T) this;
	}

	/**
	 * Converts a runtime {@link Action} into a concrete fluent action builder.
	 *
	 * @param action
	 *            the runtime action to convert
	 * @param <T>
	 *            the concrete fluent action type
	 * @return a builder that wraps metadata equivalent to {@code action}
	 */
	public static <T extends FluentAction<T>> T from(Action action) {
		ActionMetaData a = ((ActionImpl) action).toRepositoryAction();
		return from(a);
	}

	/**
	 * Converts repository action metadata into a concrete fluent action builder.
	 *
	 * @param action
	 *            the repository action metadata to convert
	 * @param <T>
	 *            the concrete fluent action type
	 * @return a fluent builder that wraps the supplied metadata
	 * @throws IllegalStateException
	 *             if the action type is not supported
	 */
	@SuppressWarnings({"unchecked", "java:S3776"}) // Complexity OK
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

	/**
	 * Sets the action name.
	 *
	 * @param name
	 *            the action name
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	/**
	 * Sets the display label shown for the action.
	 *
	 * @param displayName
	 *            the display label
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T displayName(String displayName) {
		get().setDisplayName(displayName);
		return (T) this;
	}

	/**
	 * Sets whether the display name should be escaped before rendering.
	 *
	 * @param escapeDisplayName {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public T escapeDisplayName(boolean escapeDisplayName) {
		return escapeDisplayName(escapeDisplayName ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the display name should be escaped before rendering.
	 *
	 * @param escapeDisplayName {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T escapeDisplayName(Boolean escapeDisplayName) {
		get().setEscapeDisplayName(escapeDisplayName);
		return (T) this;
	}

	/**
	 * Sets the tooltip text for the action.
	 *
	 * @param toolTip
	 *            the tooltip text
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T toolTip(String toolTip) {
		get().setToolTip(toolTip);
		return (T) this;
	}

	/**
	 * Sets whether the tooltip should be escaped before rendering.
	 *
	 * @param escapeToolTip {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public T escapeToolTip(boolean escapeToolTip) {
		return escapeToolTip(escapeToolTip ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the tooltip should be escaped before rendering.
	 *
	 * @param escapeToolTip {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T escapeToolTip(Boolean escapeToolTip) {
		get().setEscapeToolTip(escapeToolTip);
		return (T) this;
	}

	/**
	 * Sets the icon resource path used for the action.
	 *
	 * @param relativeIconFileName
	 *            the icon file path relative to the configured icon root
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T relativeIconFileName(String relativeIconFileName) {
		get().setRelativeIconFileName(relativeIconFileName);
		return (T) this;
	}

	/**
	 * Sets the CSS class for icon rendering.
	 *
	 * @param iconStyleClass
	 *            the CSS class to apply to the action icon
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T iconStyleClass(String iconStyleClass) {
		get().setIconStyleClass(iconStyleClass);
		return (T) this;
	}

	/**
	 * Sets the user confirmation text displayed before action execution.
	 *
	 * @param confirmationText
	 *            the confirmation prompt text
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T confirmationText(String confirmationText) {
		get().setConfirmationText(confirmationText);
		return (T) this;
	}

	/**
	 * Sets whether the confirmation text should be escaped before rendering.
	 *
	 * @param escapeConfirm {@code false} to allow trusted markup; {@code true} to escape at the renderer boundary
	 * @return this builder
	 */
	public T escapeConfirm(boolean escapeConfirm) {
		return escapeConfirm(escapeConfirm ? Boolean.TRUE : Boolean.FALSE);
	}

	/**
	 * Sets whether the confirmation text should be escaped before rendering.
	 *
	 * @param escapeConfirm {@code Boolean.FALSE} to allow trusted markup; {@code null} or {@code Boolean.TRUE} to escape at the renderer boundary
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T escapeConfirm(Boolean escapeConfirm) {
		get().setEscapeConfirm(escapeConfirm);
		return (T) this;
	}

	/**
	 * Sets the {@code disabled} condition as authored; the opposite sense of {@link #enabledCondition(String)}.
	 *
	 * @param disabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T disabledCondition(@Nullable String disabledCondition) {
		get().setXmlDisabledCondition(disabledCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code enabled} condition as authored; the opposite sense of {@link #disabledCondition(String)}.
	 *
	 * @param enabledCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T enabledCondition(@Nullable String enabledCondition) {
		get().setXmlEnabledCondition(enabledCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T invisibleCondition(@Nullable String invisibleCondition) {
		get().setXmlInvisibleCondition(invisibleCondition);
		return (T) this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T visibleCondition(@Nullable String visibleCondition) {
		get().setXmlVisibleCondition(visibleCondition);
		return (T) this;
	}

	/**
	 * Adds or replaces an action property entry.
	 *
	 * @param k
	 *            the property key
	 * @param v
	 *            the property value
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T putProperty(String k, String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	/**
	 * Returns the wrapped repository action instance.
	 */
	public abstract ActionMetaData get();
}
