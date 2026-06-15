package org.skyve.metadata.customer.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.customer.CustomerFeatureRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;

/**
 * Tests for customer fluent builders.
 */
@SuppressWarnings("static-method")
class FluentCustomerBuilderTest {

	// --- FluentCustomerModuleRole ---

	@Test
	void moduleRoleDefaultConstructorCreatesInstance() {
		FluentCustomerModuleRole r = new FluentCustomerModuleRole();
		assertNotNull(r.get());
	}

	@Test
	void moduleRoleWrappingConstructorUsesProvided() {
		CustomerModuleRoleMetaData md = new CustomerModuleRoleMetaData();
		FluentCustomerModuleRole r = new FluentCustomerModuleRole(md);
		assertSame(md, r.get());
	}

	@Test
	void moduleRoleModuleNameReturnsSelf() {
		FluentCustomerModuleRole r = new FluentCustomerModuleRole();
		FluentCustomerModuleRole result = r.moduleName("admin");
		assertSame(r, result);
		assertEquals("admin", r.get().getModuleName());
	}

	@Test
	void moduleRoleNameReturnsSelf() {
		FluentCustomerModuleRole r = new FluentCustomerModuleRole();
		FluentCustomerModuleRole result = r.name("BasicUser");
		assertSame(r, result);
	}

	// --- FluentCustomerFeatureRole ---

	@Test
	void featureRoleDefaultConstructorCreatesInstance() {
		FluentCustomerFeatureRole r = new FluentCustomerFeatureRole();
		assertNotNull(r.get());
	}

	@Test
	void featureRoleWrappingConstructorUsesProvided() {
		CustomerFeatureRoleMetaData md = new CustomerFeatureRoleMetaData();
		FluentCustomerFeatureRole r = new FluentCustomerFeatureRole(md);
		assertSame(md, r.get());
	}

	@Test
	void featureRoleModuleNameReturnsSelf() {
		FluentCustomerFeatureRole r = new FluentCustomerFeatureRole();
		FluentCustomerFeatureRole result = r.moduleName("admin");
		assertSame(r, result);
		assertEquals("admin", r.get().getModuleName());
	}

	@Test
	void featureRoleNameReturnsSelf() {
		FluentCustomerFeatureRole r = new FluentCustomerFeatureRole();
		FluentCustomerFeatureRole result = r.name("SuperUser");
		assertSame(r, result);
	}

	// --- FluentLoginResources ---

	@Test
	void loginResourcesDefaultConstructorCreatesInstance() {
		FluentLoginResources lr = new FluentLoginResources();
		assertNotNull(lr.get());
	}

	@Test
	void loginResourcesWrappingConstructorUsesProvided() {
		LoginResourcesMetaData md = new LoginResourcesMetaData();
		FluentLoginResources lr = new FluentLoginResources(md);
		assertSame(md, lr.get());
	}

	@Test
	void loginResourcesLoginPageURLReturnsSelf() {
		FluentLoginResources lr = new FluentLoginResources();
		FluentLoginResources result = lr.loginPageURL("/login.xhtml");
		assertSame(lr, result);
		assertEquals("/login.xhtml", lr.get().getLoginPageURL());
	}

	@Test
	void loginResourcesLoggedOutPageURLReturnsSelf() {
		FluentLoginResources lr = new FluentLoginResources();
		FluentLoginResources result = lr.loggedOutPageURL("/loggedOut.xhtml");
		assertSame(lr, result);
		assertEquals("/loggedOut.xhtml", lr.get().getLoggedOutPageURL());
	}

	// --- FluentCustomerRole ---

	@Test
	void customerRoleDefaultConstructorCreatesInstance() {
		FluentCustomerRole r = new FluentCustomerRole();
		assertNotNull(r.get());
	}

	@Test
	void customerRoleWrappingConstructorUsesProvided() {
		CustomerRoleMetaData md = new CustomerRoleMetaData();
		FluentCustomerRole r = new FluentCustomerRole(md);
		assertSame(md, r.get());
	}

	@Test
	void customerRoleNameReturnsSelf() {
		FluentCustomerRole r = new FluentCustomerRole();
		FluentCustomerRole result = r.name("Manager");
		assertSame(r, result);
		assertEquals("Manager", r.get().getName());
	}

	@Test
	void customerRoleDescriptionReturnsSelf() {
		FluentCustomerRole r = new FluentCustomerRole();
		FluentCustomerRole result = r.description("A manager role");
		assertSame(r, result);
		assertEquals("A manager role", r.get().getDescription());
	}

	@Test
	void customerRoleDocumentationReturnsSelf() {
		FluentCustomerRole r = new FluentCustomerRole();
		FluentCustomerRole result = r.documentation("some docs");
		assertSame(r, result);
		assertEquals("some docs", r.get().getDocumentation());
	}

	@Test
	void customerRoleAddRoleIncreasesCount() {
		FluentCustomerRole r = new FluentCustomerRole();
		r.addRole(new FluentCustomerModuleRole().moduleName("admin").name("BasicUser"));
		assertEquals(1, r.get().getRoles().size());
	}

	@Test
	void customerRoleFindRoleByModuleAndName() {
		FluentCustomerRole r = new FluentCustomerRole();
		r.addRole(new FluentCustomerModuleRole().moduleName("admin").name("BasicUser"));
		FluentCustomerModuleRole found = r.findRole("admin", "BasicUser");
		assertNotNull(found);
	}

	@Test
	void customerRoleFindRoleReturnsNullWhenMissing() {
		FluentCustomerRole r = new FluentCustomerRole();
		FluentCustomerModuleRole result = r.findRole("admin", "NonExistent");
		assertNull(result);
	}

	@Test
	void customerRoleRemoveRoleByModuleAndName() {
		FluentCustomerRole r = new FluentCustomerRole();
		r.addRole(new FluentCustomerModuleRole().moduleName("admin").name("BasicUser"));
		FluentCustomerRole result = r.removeRole("admin", "BasicUser");
		assertSame(r, result);
		assertEquals(0, r.get().getRoles().size());
	}

	@Test
	void customerRoleClearRolesEmptiesCollection() {
		FluentCustomerRole r = new FluentCustomerRole();
		r.addRole(new FluentCustomerModuleRole().moduleName("admin").name("BasicUser"));
		FluentCustomerRole result = r.clearRoles();
		assertSame(r, result);
		assertEquals(0, r.get().getRoles().size());
	}
}
