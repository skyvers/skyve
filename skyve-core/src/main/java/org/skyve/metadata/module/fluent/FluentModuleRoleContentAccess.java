package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleContentUserAccessMetaData;

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
	public FluentModuleRoleContentAccess(ModuleRoleContentUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleContentAccess from a runtime metadata.
	 */
	protected FluentModuleRoleContentAccess from(String documentName,
													String binding,
													Set<String> uxuis) {
		documentName(documentName);
		binding(binding);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleContentAccess.
	 */
	public FluentModuleRoleContentAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the binding for this FluentModuleRoleContentAccess.
	 */
	public FluentModuleRoleContentAccess binding(final String binding) {
		access.setBinding(binding);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRoleContentUserAccessMetaData get() {
		return access;
	}
}
