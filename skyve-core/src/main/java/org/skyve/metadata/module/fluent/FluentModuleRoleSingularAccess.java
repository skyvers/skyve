package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleSingularUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleSingularUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleSingularAccess extends
				FluentModuleRoleAccess<FluentModuleRoleSingularAccess, ModuleRoleSingularUserAccessMetaData> {
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
	 * Returns a FluentModuleRoleSingularAccess from a runtime metadata.
	 */
	protected FluentModuleRoleSingularAccess from(String documentName, Set<String> uxuis) {
		documentName(documentName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleSingularAccess.
	 */
	public FluentModuleRoleSingularAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
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
