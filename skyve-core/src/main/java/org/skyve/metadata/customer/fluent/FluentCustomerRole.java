package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.metadata.customer.CustomerRole;

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
	public FluentCustomerRole(CustomerRoleMetaData role) {
		this.role = role;
	}

	/**
	 * Copies one customer role definition into this builder.
	 *
	 * @param role source role contract
	 * @return this builder
	 */
	public FluentCustomerRole from(@SuppressWarnings("hiding") CustomerRole role) {
		name(role.getName());
		description(role.getDescription());
		documentation(role.getDocumentation());
		
		for (CustomerModuleRoleMetaData moduleRole : ((CustomerRoleMetaData) role).getRoles()) {
			addRole(new FluentCustomerModuleRole().from(moduleRole));
		}
		return this;
	}
	
	/**
	 * Sets the role name.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public FluentCustomerRole name(String name) {
		role.setName(name);
		return this;
	}

	/**
	 * Sets optional role description text.
	 *
	 * @param description role description
	 * @return this builder
	 */
	public FluentCustomerRole description(String description) {
		role.setDescription(description);
		return this;
	}

	/**
	 * Sets optional role documentation text.
	 *
	 * @param documentation role documentation
	 * @return this builder
	 */
	public FluentCustomerRole documentation(String documentation) {
		role.setDocumentation(documentation);
		return this;
	}

	/**
	 * Adds a module-role grant.
	 *
	 * @param moduleRole module-role wrapper
	 * @return this builder
	 */
	public FluentCustomerRole addRole(FluentCustomerModuleRole moduleRole) {
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
	public FluentCustomerRole removeRole(String moduleName, String name) {
		role.getRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	/**
	 * Removes all module-role grants from this customer role.
	 *
	 * @return this builder
	 */
	public FluentCustomerRole clearRoles() {
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
	public FluentCustomerModuleRole findRole(String moduleName, String name) {
		CustomerModuleRoleMetaData result = role.getRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerModuleRole(result);
		}
		return null;
	}
	
	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing customer role metadata
	 */
	public CustomerRoleMetaData get() {
		return role;
	}
}
