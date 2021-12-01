package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;

public class FluentCustomerModuleRole {
	private CustomerModuleRoleMetaData role = null;
	
	public FluentCustomerModuleRole() {
		role = new CustomerModuleRoleMetaData();
	}

	public FluentCustomerModuleRole(CustomerModuleRoleMetaData role) {
		this.role = role;
	}
	
	public FluentCustomerModuleRole from(@SuppressWarnings("hiding") CustomerModuleRoleMetaData role) {
		moduleName(role.getModuleName());
		name(role.getName());
		return this;
	}
	
	public FluentCustomerModuleRole moduleName(String moduleName) {
		role.setModuleName(moduleName);
		return this;
	}
	
	public FluentCustomerModuleRole name(String name) {
		role.setName(name);
		return this;
	}
	
	public CustomerModuleRoleMetaData get() {
		return role;
	}
}
