package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.report.ReportFormat;

public class FluentReportAction extends FluentParameterizableAction<FluentReportAction> {
	private ReportAction action = null;
	
	public FluentReportAction() {
		action = new ReportAction();
	}

	public FluentReportAction(ReportAction action) {
		this.action = action;
	}

	public FluentReportAction from(@SuppressWarnings("hiding") ReportAction action) {
		super.from(action);
		return this;
	}

	public FluentReportAction moduleName(String moduleName) {
		action.setModuleName(moduleName);
		return this;
	}

	public FluentReportAction documentName(String documentName) {
		action.setDocumentName(documentName);
		return this;
	}
	
	public FluentReportAction reportName(String reportName) {
		action.setReportName(reportName);
		return this;
	}

	public FluentReportAction reportFormat(ReportFormat reportFormat) {
		action.setReportFormat(reportFormat);
		return this;
	}

	public FluentReportAction listReport(boolean listReport) {
		action.setListReport(listReport ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentReportAction queryName(String queryName) {
		action.setQueryName(queryName);
		return this;
	}

	public FluentReportAction modelName(String modelName) {
		action.setModelName(modelName);
		return this;
	}
	
	@Override
	public ReportAction get() {
		return action;
	}
}
