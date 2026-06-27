package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerFeatureRoleMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds feature-role metadata entries used by customer feature toggles.
 */
public class FluentCustomerFeatureRole {
	private CustomerFeatureRoleMetaData role = null;
	
	/**
	 * Creates a builder with new empty feature-role metadata.
	 */
	public FluentCustomerFeatureRole() {
		role = new CustomerFeatureRoleMetaData();
	}

	/**
	 * Creates a builder around existing feature-role metadata.
	 *
	 * @param role backing metadata
	 */
	public FluentCustomerFeatureRole(@Nonnull CustomerFeatureRoleMetaData role) {
		this.role = role;
	}
	
	/**
	 * Copies an existing feature-role definition.
	 *
	 * @param role source metadata
	 * @return this builder
	 */
	public @Nonnull FluentCustomerFeatureRole from(@SuppressWarnings("hiding") @Nonnull CustomerFeatureRoleMetaData role) {
		moduleName(role.getModuleName());
		name(role.getName());
		return this;
	}
	
	/**
	 * Sets the optional module scope for this feature role.
	 *
	 * @param moduleName module name, or {@code null} for customer scope
	 * @return this builder
	 */
	public @Nonnull FluentCustomerFeatureRole moduleName(@Nullable String moduleName) {
		role.setModuleName(moduleName);
		return this;
	}
	
	/**
	 * Sets the role name.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomerFeatureRole name(@Nonnull String name) {
		role.setName(name);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing feature-role metadata
	 */
	public @Nonnull CustomerFeatureRoleMetaData get() {
		return role;
	}
}
