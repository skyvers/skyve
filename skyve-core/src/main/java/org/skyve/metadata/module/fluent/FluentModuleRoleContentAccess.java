package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleContentUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleContentUserAccessMetaData} metadata.
 * 
 * @author mike
 */
public class FluentModuleRoleContentAccess extends
				FluentModuleRoleAccess<FluentModuleRoleContentAccess, ModuleRoleContentUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleContentAccess.
	 */
	public FluentModuleRoleContentAccess() {
		access = new ModuleRoleContentUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleContentAccess from the specified ModuleRoleContentUserAccessMetaData.
	 */
	public FluentModuleRoleContentAccess(@Nonnull ModuleRoleContentUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleContentAccess from a runtime metadata.
	 */
	protected @Nonnull FluentModuleRoleContentAccess from(@Nonnull String documentName,
													@Nonnull String binding,
													@Nonnull Set<String> uxuis) {
		documentName(documentName);
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleContentAccess.
	 */
	public @Nonnull FluentModuleRoleContentAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the binding for this FluentModuleRoleContentAccess.
	 */
	public @Nonnull FluentModuleRoleContentAccess binding(final @Nonnull String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleContentUserAccessMetaData get() {
		return access;
	}
}
