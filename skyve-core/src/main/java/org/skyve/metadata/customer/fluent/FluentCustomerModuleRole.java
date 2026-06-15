package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;

/**
 * Builds module-scoped role entries nested within a customer role.
 */
public class FluentCustomerModuleRole {
	private CustomerModuleRoleMetaData role = null;
	
	/**
	 * Creates a builder with new empty module-role metadata.
	 */
	public FluentCustomerModuleRole() {
		role = new CustomerModuleRoleMetaData();
	}

	/**
	 * Creates a builder around existing module-role metadata.
	 *
	 * @param role backing metadata
	 */
	public FluentCustomerModuleRole(CustomerModuleRoleMetaData role) {
		this.role = role;
	}
	
	/**
	 * Copies an existing module-role entry into this builder.
	 *
	 * @param role source metadata
	 * @return this builder
	 */
	public FluentCustomerModuleRole from(@SuppressWarnings("hiding") CustomerModuleRoleMetaData role) {
		moduleName(role.getModuleName());
		name(role.getName());
		return this;
	}
	
	/**
	 * Sets the module name for this role grant.
	 *
	 * @param moduleName module name
	 * @return this builder
	 */
	public FluentCustomerModuleRole moduleName(String moduleName) {
		role.setModuleName(moduleName);
		return this;
	}
	
	/**
	 * Sets the role name.
	 *
	 * @param name role name
	 * @return this builder
	 */
	public FluentCustomerModuleRole name(String name) {
		role.setName(name);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing module-role metadata
	 */
	public CustomerModuleRoleMetaData get() {
		return role;
	}
}
