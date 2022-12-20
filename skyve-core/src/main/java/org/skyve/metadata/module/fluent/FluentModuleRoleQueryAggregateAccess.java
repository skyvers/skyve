package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRoleQueryAggregateUserAccessMetaData;

/**
 * A fluent helper builder class to construct and manipulate the {@link ModuleRoleQueryAggregateUserAccessMetaData} metadata.
 * 
 * @author benpetito
 */
public class FluentModuleRoleQueryAggregateAccess
		extends FluentModuleRoleAccess<FluentModuleRoleQueryAggregateAccess, ModuleRoleQueryAggregateUserAccessMetaData> {

	/**
	 * Creates a new FluentModuleRoleQueryAggregateAccess.
	 */
	public FluentModuleRoleQueryAggregateAccess() {
		access = new ModuleRoleQueryAggregateUserAccessMetaData();
	}

	/**
	 * Creates a new FluentModuleRoleQueryAggregateAccess from the specified ModuleRoleQueryAggregateUserAccessMetaData.
	 */
	public FluentModuleRoleQueryAggregateAccess(ModuleRoleQueryAggregateUserAccessMetaData access) {
		this.access = access;
	}

	/**
	 * Returns a FluentModuleRoleQueryAggregateAccess from a ModuleRoleQueryAggregateUserAccessMetaData.
	 */
	@Override
	protected FluentModuleRoleQueryAggregateAccess from(
			@SuppressWarnings("hiding") ModuleRoleQueryAggregateUserAccessMetaData access) {
		super.from(access);
		queryName(access.getQueryName());
		return this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	@Override
	public ModuleRoleQueryAggregateUserAccessMetaData get() {
		return access;
	}

	/**
	 * Specifies the query name for this FluentModuleRoleQueryAggregateAccess.
	 */
	public FluentModuleRoleQueryAggregateAccess queryName(final String queryName) {
		access.setQueryName(queryName);
		return this;
	}
}
