package org.skyve.metadata.customer.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.customer.CustomerRolesMetaData;

class FluentCustomerRolesTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		assertNotNull(r.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesSupplied() {
		CustomerRolesMetaData meta = new CustomerRolesMetaData();
		FluentCustomerRoles r = new FluentCustomerRoles(meta);
		assertSame(meta, r.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void allowModuleRolesSetsFlag() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		FluentCustomerRoles result = r.allowModuleRoles(false);
		assertSame(r, result);
		assertFalse(r.get().isAllowModuleRoles());
	}

	@Test
	@SuppressWarnings("static-method")
	void addRoleAddsToCollection() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		FluentCustomerRole role = new FluentCustomerRole().name("Admin");
		FluentCustomerRoles result = r.addRole(role);
		assertSame(r, result);
		assertEquals(1, r.get().getRoles().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void findRoleReturnsMatch() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		r.addRole(new FluentCustomerRole().name("Admin"));
		FluentCustomerRole found = r.findRole("Admin");
		assertNotNull(found);
		assertEquals("Admin", found.get().getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void findRoleReturnsNullWhenMissing() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		assertNull(r.findRole("NoSuchRole"));
	}

	@Test
	@SuppressWarnings("static-method")
	void removeRoleRemovesMatchingRole() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		r.addRole(new FluentCustomerRole().name("Admin"));
		FluentCustomerRoles result = r.removeRole("Admin");
		assertSame(r, result);
		assertTrue(r.get().getRoles().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void clearRolesEmptiesCollection() {
		FluentCustomerRoles r = new FluentCustomerRoles();
		r.addRole(new FluentCustomerRole().name("Admin"));
		r.addRole(new FluentCustomerRole().name("Manager"));
		FluentCustomerRoles result = r.clearRoles();
		assertSame(r, result);
		assertTrue(r.get().getRoles().isEmpty());
	}
}
