package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleReportUserAccessMetaData;

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
	public FluentModuleRoleReportAccess(ModuleRoleReportUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleReportAccess from a runtime metadata.
	 */
	protected FluentModuleRoleReportAccess from(String moduleName, 
													String documentName,
													String reportName,
													Set<String> uxuis) {
		moduleName(moduleName);
		documentName(documentName);
		reportName(reportName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the module name for this FluentModuleRoleReportAccess.
	 */
	public FluentModuleRoleReportAccess moduleName(final String moduleName) {
		access.setModuleName(moduleName);
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleReportAccess.
	 */
	public FluentModuleRoleReportAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the report name for this FluentModuleRoleReportAccess.
	 */
	public FluentModuleRoleReportAccess reportName(final String reportName) {
		access.setReportName(reportName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRoleReportUserAccessMetaData get() {
		return access;
	}
}
