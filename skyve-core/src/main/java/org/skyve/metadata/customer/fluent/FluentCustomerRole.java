package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.metadata.customer.CustomerRole;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds one customer role and its module-role grants.
 */
public class FluentCustomerRole {
	private CustomerRoleMetaData role = null;
	
	/**
	 * Creates a builder with new empty customer-role metadata.
	 */
	public FluentCustomerRole() {
		role = new CustomerRoleMetaData();
	}
	
	/**
	 * Creates a builder around existing customer-role metadata.
	 *
	 * @param role backing metadata
	 */
	public FluentCustomerRole(@Nonnull CustomerRoleMetaData role) {
		this.role = role;
	}

	/**
	 * Copies one customer role definition into this builder.
	 *
	 * @param role source role contract
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole from(@SuppressWarnings("hiding") @Nonnull CustomerRole role) {
		name(role.getName());
		description(role.getDescription());
		documentation(role.getDocumentation());
		
		for (CustomerModuleRoleMetaData moduleRole : ((CustomerRoleMetaData) role).getRoles()) {
			addRole(new FluentCustomerModuleRole().from(moduleRole));
		}
		role.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}
	
	/**
	 * Sets the role name.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole name(@Nullable String name) {
		role.setName(name);
		return this;
	}

	/**
	 * Sets optional role description text.
	 *
	 * @param description role description
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole description(@Nullable String description) {
		role.setDescription(description);
		return this;
	}

	/**
	 * Sets optional role documentation text.
	 *
	 * @param documentation role documentation
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole documentation(@Nullable String documentation) {
		role.setDocumentation(documentation);
		return this;
	}

	/**
	 * Adds a module-role grant.
	 *
	 * @param moduleRole module-role wrapper
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole addRole(@Nonnull FluentCustomerModuleRole moduleRole) {
		role.getRoles().add(moduleRole.get());
		return this;
	}
	
	/**
	 * Removes a module-role grant by module and role name.
	 *
	 * @param moduleName module name
	 * @param name role name
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole removeRole(@Nonnull String moduleName, @Nonnull String name) {
		role.getRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all module-role grants from this customer role.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole clearRoles() {
		role.getRoles().clear();
		return this;
	}
	
	/**
	 * Finds a module-role grant by module and role name.
	 *
	 * @param moduleName module name
	 * @param name module role name
	 * @return matching module-role wrapper, or {@code null}
	 */
	public @Nullable FluentCustomerModuleRole findRole(@Nonnull String moduleName, @Nonnull String name) {
		CustomerModuleRoleMetaData result = role.getRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerModuleRole(result);
		}
		return null;
	}
	
	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentCustomerRole putProperty(@Nonnull String k, @Nonnull String v) {
		role.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing customer role metadata
	 */
	public @Nonnull CustomerRoleMetaData get() {
		return role;
	}
}
