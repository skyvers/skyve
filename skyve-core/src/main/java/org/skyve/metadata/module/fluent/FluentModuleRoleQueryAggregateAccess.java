package org.skyve.metadata.module.fluent;

import java.util.Set;

import org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData;

import jakarta.annotation.Nonnull;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleQueryAggregateUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleQueryAggregateAccess extends
				FluentModuleRoleAccess<FluentModuleRoleQueryAggregateAccess, ModuleRoleQueryAggregateUserAccessMetaData> {
	/**
	 * Creates a new FluentModuleRoleQueryAggregateAccess.
	 */
	public FluentModuleRoleQueryAggregateAccess() {
		access = new ModuleRoleQueryAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleQueryAggregateAccess from the specified ModuleRoleQueryAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleQueryAggregateAccess(@Nonnull ModuleRoleQueryAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleQueryAggregateAccess from a runtime metadata.
	 */
	protected @Nonnull FluentModuleRoleQueryAggregateAccess from(@Nonnull String queryName, @Nonnull Set<String> uxuis) {
		queryName(queryName);
		uxuis.forEach(u -> addUxUi(u));
		return this;
	}

	/**
	 * Specifies the query name for this FluentModuleRoleQueryAggregateAccess.
	 */
	public @Nonnull FluentModuleRoleQueryAggregateAccess queryName(final @Nonnull String queryName) {
		access.setQueryName(queryName);
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public @Nonnull ModuleRoleQueryAggregateUserAccessMetaData get() {
		return access;
	}
}
