package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRolePreviousCompleteUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRolePreviousCompleteAccess
		extends FluentModuleRoleAccess<FluentModuleRolePreviousCompleteAccess, ModuleRolePreviousCompleteUserAccessMetaData> {

	/**
	 * Creates a new FluentModuleRolePreviousCompleteAccess.
	 */
	public FluentModuleRolePreviousCompleteAccess() {
		access = new ModuleRolePreviousCompleteUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRolePreviousCompleteAccess from the specified ModuleRolePreviousCompleteUserAccessMetaData.
	 */
	public FluentModuleRolePreviousCompleteAccess(ModuleRolePreviousCompleteUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Specifies the binding for this FluentModuleRolePreviousCompleteAccess.
	 */
	public FluentModuleRolePreviousCompleteAccess binding(final String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRolePreviousCompleteAccess.
	 */
	public FluentModuleRolePreviousCompleteAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns a FluentModuleRolePreviousCompleteAccess from a ModuleRolePreviousCompleteUserAccessMetaData.
	 */
	@Override
	protected FluentModuleRolePreviousCompleteAccess from(
			@SuppressWarnings("hiding") ModuleRolePreviousCompleteUserAccessMetaData access) {
		super.from(access);
		documentName(access.getDocumentName());
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRolePreviousCompleteUserAccessMetaData get() {
		return access;
	}
}
