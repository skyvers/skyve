package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;

public class FluentCustomerModuleRole {
	private CustomerModuleRoleMetaData role = new CustomerModuleRoleMetaData();
	
	public FluentCustomerModuleRole() {
		// nothing to see
	}
	
	public FluentCustomerModuleRole(CustomerModuleRoleMetaData role) {
		moduleName(role.getModuleName());
		name(role.getName());
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
