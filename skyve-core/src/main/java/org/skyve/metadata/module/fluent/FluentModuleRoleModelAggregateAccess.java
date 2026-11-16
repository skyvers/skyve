package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleModelAggregateUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleModelAggregateUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleModelAggregateAccess extends
				FluentModuleRoleAccess<FluentModuleRoleModelAggregateAccess, ModuleRoleModelAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleModelAggregateAccess.
	 */
	public FluentModuleRoleModelAggregateAccess() {
		access = new ModuleRoleModelAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleModelAggregateAccess from the specified ModuleRoleModelAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleModelAggregateAccess(@Nonnull ModuleRoleModelAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleModelAggregateAccess from runtime metadata.
	 */
	protected @Nonnull FluentModuleRoleModelAggregateAccess from(@Nonnull String documentName, @Nonnull String modelName, @Nonnull Set<String> uxuis) {
		documentName(documentName);
		modelName(modelName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleModelAggregateAccess.
	 */
	public @Nonnull FluentModuleRoleModelAggregateAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Specifies the model name for this FluentModuleRoleModelAggregateAccess.
	 */
	public @Nonnull FluentModuleRoleModelAggregateAccess modelName(final @Nonnull String modelName) {
		access.setModelName(modelName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleModelAggregateUserAccessMetaData get() {
		return access;
	}
}
