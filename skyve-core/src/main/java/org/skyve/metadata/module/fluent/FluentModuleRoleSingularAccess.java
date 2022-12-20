package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleSingularUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleSingularAccess
		extends FluentModuleRoleAccess<FluentModuleRoleSingularAccess, ModuleRoleSingularUserAccessMetaData> {

	/**
	 * Creates a new FluentModuleRoleSingularAccess.
	 */
	public FluentModuleRoleSingularAccess() {
		access = new ModuleRoleSingularUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleSingularAccess from the specified ModuleRoleDocumentAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleSingularAccess(ModuleRoleSingularUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleSingularAccess.
	 */
	public FluentModuleRoleSingularAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns a FluentModuleRoleSingularAccess from a ModuleRoleDocumentAggregateUserAccessMetaData.
	 */
	@Override
	protected FluentModuleRoleSingularAccess from(@SuppressWarnings("hiding") ModuleRoleSingularUserAccessMetaData access) {
		super.from(access);
		documentName(access.getDocumentName());

		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRoleSingularUserAccessMetaData get() {
		return access;
	}
}
