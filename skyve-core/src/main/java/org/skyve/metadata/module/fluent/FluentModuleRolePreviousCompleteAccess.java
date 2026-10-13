package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRolePreviousCompleteUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRolePreviousCompleteAccess extends
				FluentModuleRoleAccess<FluentModuleRolePreviousCompleteAccess, ModuleRolePreviousCompleteUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRolePreviousCompleteAccess.
	 */
	public FluentModuleRolePreviousCompleteAccess() {
		access = new ModuleRolePreviousCompleteUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRolePreviousCompleteAccess from the specified ModuleRolePreviousCompleteUserAccessMetaData.
	 */
	public FluentModuleRolePreviousCompleteAccess(@Nonnull ModuleRolePreviousCompleteUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRolePreviousCompleteAccess from a runtime metadata.
	 */
	protected @Nonnull FluentModuleRolePreviousCompleteAccess from(@Nonnull String documentName,
															@Nonnull String binding,
															@Nonnull Set<String> uxuis) {
		documentName(documentName);
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRolePreviousCompleteAccess.
	 */
	public @Nonnull FluentModuleRolePreviousCompleteAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the binding for this FluentModuleRolePreviousCompleteAccess.
	 */
	public @Nonnull FluentModuleRolePreviousCompleteAccess binding(final @Nonnull String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRolePreviousCompleteUserAccessMetaData get() {
		return access;
	}
}
