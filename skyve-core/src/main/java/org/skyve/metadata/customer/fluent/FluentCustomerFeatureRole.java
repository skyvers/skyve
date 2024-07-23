package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerFeatureRoleMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class FluentCustomerFeatureRole {
	private CustomerFeatureRoleMetaData role = null;
	
	public FluentCustomerFeatureRole() {
		role = new CustomerFeatureRoleMetaData();
	}

	public FluentCustomerFeatureRole(@Nonnull CustomerFeatureRoleMetaData role) {
		this.role = role;
	}
	
	public @Nonnull FluentCustomerFeatureRole from(@SuppressWarnings("hiding") @Nonnull CustomerFeatureRoleMetaData role) {
		moduleName(role.getModuleName());
		name(role.getName());
		return this;
	}
	
	public @Nonnull FluentCustomerFeatureRole moduleName(@Nullable String moduleName) {
		role.setModuleName(moduleName);
		return this;
	}
	
	public @Nonnull FluentCustomerFeatureRole name(@Nonnull String name) {
		role.setName(name);
		return this;
	}
	
	public @Nonnull CustomerFeatureRoleMetaData get() {
		return role;
	}
}
