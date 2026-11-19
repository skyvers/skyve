package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleDocumentAggregateUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleDocumentAggregateUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleDocumentAggregateAccess extends 
				FluentModuleRoleAccess<FluentModuleRoleDocumentAggregateAccess, ModuleRoleDocumentAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleDocumentAggregateAccess.
	 */
	public FluentModuleRoleDocumentAggregateAccess() {
		access = new ModuleRoleDocumentAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleDocumentAggregateAccess from the specified ModuleRoleDocumentAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleDocumentAggregateAccess(@Nonnull ModuleRoleDocumentAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleDocumentAggregateAccess from a runtime metadata.
	 */
	protected @Nonnull FluentModuleRoleDocumentAggregateAccess from(@Nonnull String documentName, @Nonnull Set<String> uxuis) {
		documentName(documentName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the document name for this FluentModuleRoleDocumentAggregateAccess.
	 */
	public @Nonnull FluentModuleRoleDocumentAggregateAccess documentName(final @Nonnull String documentName) {
		access.setDocumentName(documentName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleDocumentAggregateUserAccessMetaData get() {
		return access;
	}
}
