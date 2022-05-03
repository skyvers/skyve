package org.skyve.metadata.customer.fluent;

import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.metadata.customer.CustomerRole;

public class FluentCustomerRole {
	private CustomerRoleMetaData role = null;
	
	public FluentCustomerRole() {
		role = new CustomerRoleMetaData();
	}
	
	public FluentCustomerRole(CustomerRoleMetaData role) {
		this.role = role;
	}

	public FluentCustomerRole from(@SuppressWarnings("hiding") CustomerRole role) {
		name(role.getName());
		description(role.getDescription());
		documentation(role.getDocumentation());
		
		for (CustomerModuleRoleMetaData moduleRole : ((CustomerRoleMetaData) role).getRoles()) {
			addRole(new FluentCustomerModuleRole().from(moduleRole));
		}
		return this;
	}
	
	public FluentCustomerRole name(String name) {
		role.setName(name);
		return this;
	}

	public FluentCustomerRole description(String description) {
		role.setDescription(description);
		return this;
	}

	public FluentCustomerRole documentation(String documentation) {
		role.setDocumentation(documentation);
		return this;
	}

	public FluentCustomerRole addRole(FluentCustomerModuleRole moduleRole) {
		role.getRoles().add(moduleRole.get());
		return this;
	}
	
	public FluentCustomerRole removeRole(String moduleName, String name) {
		role.getRoles().removeIf(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName()));
		return this;
	}

	public FluentCustomerRole clearRoles() {
		role.getRoles().clear();
		return this;
	}
	
	public FluentCustomerModuleRole findRole(String moduleName, String name) {
		CustomerModuleRoleMetaData result = role.getRoles().stream().filter(r -> moduleName.equals(r.getModuleName()) && name.equals(r.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCustomerModuleRole(result);
		}
		return null;
	}
	
	public CustomerRoleMetaData get() {
		return role;
	}
}
