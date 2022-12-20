package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRoleModelAggregateUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleModelAggregateUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleModelAggregateAccess
		extends FluentModuleRoleAccess<FluentModuleRoleModelAggregateAccess, ModuleRoleModelAggregateUserAccessMetaData> {

	/**
	 * Creates a new FluentModuleRoleModelAggregateAccess.
	 */
	public FluentModuleRoleModelAggregateAccess() {
		access = new ModuleRoleModelAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleModelAggregateAccess from the specified ModuleRoleModelAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleModelAggregateAccess(ModuleRoleModelAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleModelAggregateAccess.
	 */
	public FluentModuleRoleModelAggregateAccess documentName(final String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns a FluentModuleRoleModelAggregateAccess from a ModuleRoleModelAggregateUserAccessMetaData.
	 */
	@Override
	protected FluentModuleRoleModelAggregateAccess from(
			@SuppressWarnings("hiding") ModuleRoleModelAggregateUserAccessMetaData access) {
		super.from(access);
		documentName(access.getDocumentName());
		modelName(access.getModelName());
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRoleModelAggregateUserAccessMetaData get() {
		return access;
	}

	/**
	 * Specifies the model name for this FluentModuleRoleModelAggregateAccess.
	 */
	public FluentModuleRoleModelAggregateAccess modelName(final String modelName) {
		access.setModelName(modelName);
		return this;
	}
}
