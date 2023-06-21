package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRolePreviousCompleteUserAccessMetaData;

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
	public FluentModuleRolePreviousCompleteAccess(ModuleRolePreviousCompleteUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRolePreviousCompleteAccess from a runtime metadata.
	 */
	protected FluentModuleRolePreviousCompleteAccess from(String documentName,
															String binding,
															Set<String> uxuis) {
		documentName(documentName);
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
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
	 * Specifies the binding for this FluentModuleRolePreviousCompleteAccess.
	 */
	public FluentModuleRolePreviousCompleteAccess binding(final String binding) {
		access.setBinding(binding);
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
