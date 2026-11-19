package org.skyve.metadata.view.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.view.access.ViewReportUserAccessMetaData;

import jakarta.annotation.Nonnull;

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
	public FluentViewReportAccess(@Nonnull ViewReportUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentViewReportAccess from a runtime metadata.
	 */
	protected @Nonnull FluentViewReportAccess from(@Nonnull String moduleName, @Nonnull String documentName, @Nonnull String reportName, @Nonnull Set<String> uxuis) {
		moduleName(moduleName);
		documentName(documentName);
		reportName(reportName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the module name for this FluentViewReportAccess.
	 */
	public @Nonnull FluentViewReportAccess moduleName(final @Nonnull String moduleName) {
		access.setModuleName(moduleName);
		return this;
	}

	/**
	 * Specifies the document name for this FluentViewReportAccess.
	 */
	public @Nonnull FluentViewReportAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the report name for this FluentViewReportAccess.
	 */
	public @Nonnull FluentViewReportAccess reportName(final @Nonnull String reportName) {
		access.setReportName(reportName);
		return this;
	}

	/**
	 * Returns the underlying model aggregate user access metadata from this builder.
	 */
	@Override
	public @Nonnull ViewReportUserAccessMetaData get() {
		return access;
	}
}
