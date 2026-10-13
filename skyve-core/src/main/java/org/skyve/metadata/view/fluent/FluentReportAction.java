package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.report.ReportFormat;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link ReportAction} metadata using a fluent API.
 */
public class FluentReportAction extends FluentParameterizableAction<FluentReportAction> {
	private ReportAction action = null;
	
	/**
	 * Creates a fluent builder backed by a new {@link ReportAction} instance.
	 */
	public FluentReportAction() {
		action = new ReportAction();
	}

	/**
	 * Creates a fluent builder backed by the supplied action instance.
	 *
	 * @param action
	 *            the action instance to wrap
	 */
	public FluentReportAction(@Nonnull ReportAction action) {
		this.action = action;
	}

	/**
	 * Copies repository metadata values into this builder.
	 *
	 * @param action
	 *            the source action metadata
	 * @return this builder
	 */
	public @Nonnull FluentReportAction from(@SuppressWarnings("hiding") @Nonnull ReportAction action) {
		super.from(action);
		return this;
	}

	/**
	 * Sets the module that owns the report definition.
	 *
	 * @param moduleName
	 *            the owning module name
	 * @return this builder
	 */
	public @Nonnull FluentReportAction moduleName(@Nullable String moduleName) {
		action.setModuleName(moduleName);
		return this;
	}

	/**
	 * Sets the document that scopes the report definition.
	 *
	 * @param documentName
	 *            the owning document name
	 * @return this builder
	 */
	public @Nonnull FluentReportAction documentName(@Nullable String documentName) {
		action.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the report metadata name to execute.
	 *
	 * @param reportName
	 *            the report name
	 * @return this builder
	 */
	public @Nonnull FluentReportAction reportName(@Nullable String reportName) {
		action.setReportName(reportName);
		return this;
	}

	/**
	 * Sets the output format used when rendering the report.
	 *
	 * @param reportFormat
	 *            the desired report format
	 * @return this builder
	 */
	public @Nonnull FluentReportAction reportFormat(@Nullable ReportFormat reportFormat) {
		action.setReportFormat(reportFormat);
		return this;
	}

	/**
	 * Sets whether the action executes as a list report.
	 *
	 * @param listReport
	 *            whether the report should run against list context
	 * @return this builder
	 */
	public @Nonnull FluentReportAction listReport(boolean listReport) {
		action.setListReport(listReport ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Sets the query name used to source report rows.
	 *
	 * @param queryName
	 *            the query metadata name
	 * @return this builder
	 */
	public @Nonnull FluentReportAction queryName(@Nullable String queryName) {
		action.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model name used to source report data.
	 *
	 * @param modelName
	 *            the model metadata name
	 * @return this builder
	 */
	public @Nonnull FluentReportAction modelName(@Nullable String modelName) {
		action.setModelName(modelName);
		return this;
	}
	
	/**
	 * Returns the wrapped repository action instance.
	 */
	@Override
	public @Nonnull ReportAction get() {
		return action;
	}
}
