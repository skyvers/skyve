package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class RoleImplTest {

	@Test
	@SuppressWarnings("static-method")
	void setNameRoundtrip() {
		RoleImpl role = new RoleImpl();
		role.setName("ViewerRole");
		assertThat(role.getName(), is("ViewerRole"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDescriptionRoundtrip() {
		RoleImpl role = new RoleImpl();
		role.setDescription("Can view all data");
		assertThat(role.getDescription(), is("Can view all data"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentationRoundtrip() {
		RoleImpl role = new RoleImpl();
		role.setDocumentation("Role docs");
		assertThat(role.getDocumentation(), is("Role docs"));
	}

	@Test
	@SuppressWarnings("static-method")
	void privilegesInitiallyEmpty() {
		RoleImpl role = new RoleImpl();
		assertNotNull(role.getPrivileges());
		assertTrue(role.getPrivileges().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentRestrictionsInitiallyEmpty() {
		RoleImpl role = new RoleImpl();
		assertNotNull(role.getContentRestrictions());
		assertTrue(role.getContentRestrictions().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void contentPermissionsInitiallyEmpty() {
		RoleImpl role = new RoleImpl();
		assertNotNull(role.getContentPermissions());
		assertTrue(role.getContentPermissions().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void accessesInitiallyEmpty() {
		RoleImpl role = new RoleImpl();
		assertNotNull(role.getAccesses());
		assertTrue(role.getAccesses().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void propertiesNotNull() {
		RoleImpl role = new RoleImpl();
		assertNotNull(role.getProperties());
	}

	@Test
	@SuppressWarnings("static-method")
	void owningModuleNullByDefault() {
		RoleImpl role = new RoleImpl();
		assertThat(role.getOwningModule(), is((org.skyve.metadata.module.Module) null));
	}
}
