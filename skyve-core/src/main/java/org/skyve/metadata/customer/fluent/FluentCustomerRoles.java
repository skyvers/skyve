package org.skyve.metadata.customer.fluent;

import java.util.Collection;

import org.skyve.impl.metadata.repository.customer.CustomerRolesMetaData;
import org.skyve.metadata.customer.CustomerRole;

public class FluentCustomerRoles {
	private CustomerRolesMetaData roles = null;
	
	public FluentCustomerRoles() {
		roles = new CustomerRolesMetaData();
	}
	
	public FluentCustomerRoles(CustomerRolesMetaData roles) {
		this.roles = roles;
	}

	public FluentCustomerRoles from(@SuppressWarnings("hiding") Collection<CustomerRole> roles,
										boolean allowModuleRoles) {
		allowModuleRoles(allowModuleRoles);
		for (CustomerRole role : roles) {
			addRole(new FluentCustomerRole().from(role));
		}
		return this;
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
