package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleDynamicImageUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleDynamicImageUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentModuleReportAccess extends
				FluentModuleRoleAccess<FluentModuleReportAccess, ModuleRoleDynamicImageUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleDynamicImageAccess.
	 */
	public FluentModuleReportAccess() {
		access = new ModuleRoleDynamicImageUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleDynamicImageAccess from the specified ModuleRoleDynamicImageUserAccessMetaData.
	 */
	public FluentModuleReportAccess(@Nonnull ModuleRoleDynamicImageUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleDynamicImageAccess from runtime metadata.
	 */
	protected @Nonnull FluentModuleReportAccess from(@Nonnull String documentName, @Nonnull String imageName, @Nonnull Set<String> uxuis) {
		documentName(documentName);
		imageName(imageName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleDynamicImageAccess.
	 */
	public @Nonnull FluentModuleReportAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the image name for this FluentModuleRoleDynamicImageAccess.
	 */
	public @Nonnull FluentModuleReportAccess imageName(final @Nonnull String imageName) {
		access.setImageName(imageName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleDynamicImageUserAccessMetaData get() {
		return access;
	}
}
