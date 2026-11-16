package org.skyve.metadata.customer.fluent;

import java.util.Collection;

import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRolesMetaData;
import org.skyve.metadata.customer.CustomerRole;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds the customer role collection metadata.
 */
public class FluentCustomerRoles {
	private CustomerRolesMetaData roles = null;
	
	/**
	 * Creates a builder with new empty customer-roles metadata.
	 */
	public FluentCustomerRoles() {
		roles = new CustomerRolesMetaData();
	}
	
	/**
	 * Creates a builder around existing customer-roles metadata.
	 *
	 * @param roles backing metadata
	 */
	public FluentCustomerRoles(@Nonnull CustomerRolesMetaData roles) {
		this.roles = roles;
	}

	/**
	 * Copies roles and role-policy flags from an existing customer contract.
	 *
	 * @param roles source roles
	 * @param allowModuleRoles whether module roles are enabled
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRoles from(@SuppressWarnings("hiding") @Nonnull Collection<CustomerRole> roles,
												boolean allowModuleRoles) {
		allowModuleRoles(allowModuleRoles);
		for (CustomerRole role : roles) {
			addRole(new FluentCustomerRole().from(role));
		}
		return this;
	}

	/**
	 * Sets whether roles may include module-role grants.
	 *
	 * @param allow whether module roles are allowed
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRoles allowModuleRoles(boolean allow) {
		roles.setAllowModuleRoles(allow);
		return this;
	}

	/**
	 * Adds one customer role.
	 *
	 * @param role role wrapper to add
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRoles addRole(@Nonnull FluentCustomerRole role) {
		roles.getRoles().add(role.get());
		return this;
	}

	/**
	 * Removes a customer role by name.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRoles removeRole(@Nonnull String name) {
		roles.getRoles().removeIf(r -> name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all customer roles.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRoles clearRoles() {
		roles.getRoles().clear();
		return this;
	}
	
	/**
	 * Finds a role by name.
	 *
	 * @param name role name
	 * @return a fluent wrapper when found, otherwise {@code null}
	 */
	public @Nullable FluentCustomerRole findRole(@Nonnull String name) {
		CustomerRoleMetaData result = roles.getRoles().stream().filter(r -> name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerRole(result);
		}
		return null;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing roles metadata
	 */
	public @Nonnull CustomerRolesMetaData get() {
		return roles;
	}
}
