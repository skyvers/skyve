package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleReportUserAccessMetaData;
import org.skyve.metadata.model.document.ModuleDocument;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleReportUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentModuleRoleReportAccess extends
				FluentModuleRoleAccess<FluentModuleRoleReportAccess, ModuleRoleReportUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleReportAccess.
	 */
	public FluentModuleRoleReportAccess() {
		access = new ModuleRoleReportUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleReportAccess from the specified ModuleRoleReportUserAccessMetaData.
	 */
	public FluentModuleRoleReportAccess(@Nonnull ModuleRoleReportUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleReportAccess from a runtime metadata.
	 */
	protected @Nonnull FluentModuleRoleReportAccess from(@Nonnull String moduleName, 
															@Nonnull String documentName,
															@Nonnull String reportName,
															@Nonnull Set<String> uxuis) {
		moduleName(moduleName);
		documentName(documentName);
		reportName(reportName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Returns a FluentModuleRoleReportAccess from a runtime metadata; a typed shortcut for
	 * {@link #from(String, String, String, Set)}.
	 */
	protected @Nonnull FluentModuleRoleReportAccess from(@Nonnull ModuleDocument document,
															@Nonnull String reportName,
															@Nonnull Set<String> uxuis) {
		return from(document.moduleName(), document.documentName(), reportName, uxuis);
	}

	/**
	 * Specifies the module name for this FluentModuleRoleReportAccess.
	 */
	public @Nonnull FluentModuleRoleReportAccess moduleName(final @Nonnull String moduleName) {
		access.setModuleName(moduleName);
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleReportAccess.
	 */
	public @Nonnull FluentModuleRoleReportAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the report name for this FluentModuleRoleReportAccess.
	 */
	public @Nonnull FluentModuleRoleReportAccess reportName(final @Nonnull String reportName) {
		access.setReportName(reportName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleReportUserAccessMetaData get() {
		return access;
	}
}
