package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewReportUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ViewReportUserAccessMetaData} metadata.
 * 
 * @author brandon-klar
 */
public class FluentViewReportAccess extends FluentViewUserAccess<FluentViewReportAccess, ViewReportUserAccessMetaData> {
	/**
	 * Creates a new FluentViewReportAccess
	 */
	public FluentViewReportAccess() {
		access = new ViewReportUserAccessMetaData();
	}

	/**
	 * Creates a new FluentViewReportAccess from the specified ViewReportUserAccessMetaData.
	 */
	public FluentViewReportAccess(ViewReportUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewReportAccess from a runtime metadata.
	 */
	protected FluentViewReportAccess from(String moduleName, String documentName, String reportName, Set<String> uxuis) {
		moduleName(moduleName);
		documentName(documentName);
		reportName(reportName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the module name for this FluentViewReportAccess.
	 */
	public FluentViewReportAccess moduleName(final String moduleName) {
		access.setModuleName(moduleName);
		return this;
	}

	/**
	 * Specifies the document name for this FluentViewReportAccess.
	 */
	public FluentViewReportAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the report name for this FluentViewReportAccess.
	 */
	public FluentViewReportAccess reportName(final String reportName) {
		access.setReportName(reportName);
		return this;
	}

	/**
	 * Returns the underlying model aggregate user access metadata from this builder.
	 */
	@Override
	public ViewReportUserAccessMetaData get() {
		return access;
	}
}
