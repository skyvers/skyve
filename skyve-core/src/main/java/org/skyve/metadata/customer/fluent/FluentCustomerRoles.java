package org.skyve.metadata.customer.fluent;

import java.util.Collection;

import org.skyve.impl.metadata.repository.customer.CustomerRolesMetaData;
import org.skyve.metadata.customer.CustomerRole;

public class FluentCustomerRoles {
	private CustomerRolesMetaData roles = new CustomerRolesMetaData();
	
	public FluentCustomerRoles() {
		// nothing to see
	}
	
	public FluentCustomerRoles(Collection<CustomerRole> roles, boolean allowModuleRoles) {
		allowModuleRoles(allowModuleRoles);
		for (CustomerRole role : roles) {
			addRole(new FluentCustomerRole(role));
		}
	}

	public FluentCustomerRoles allowModuleRoles(boolean allow) {
		roles.setAllowModuleRoles(allow);
		return this;
	}

	public FluentCustomerRoles addRole(FluentCustomerRole role) {
		roles.getRoles().add(role.get());
		return this;
	}

	public CustomerRolesMetaData get() {
		return roles;
	}
}
