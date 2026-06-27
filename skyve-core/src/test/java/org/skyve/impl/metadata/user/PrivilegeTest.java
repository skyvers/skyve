package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.user.DocumentPermission;

class PrivilegeTest {

	@Test
	@SuppressWarnings("static-method")
	void setNameRoundtrip() {
		DocumentPrivilege p = new DocumentPrivilege();
		p.setName("User");
		assertThat(p.getName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void nameNullByDefault() {
		DocumentPrivilege p = new DocumentPrivilege();
		assertNull(p.getName());
	}

	@Test
	@SuppressWarnings("static-method")
	void propertiesNotNull() {
		DocumentPrivilege p = new DocumentPrivilege();
		assertNotNull(p.getProperties());
	}

	@Test
	@SuppressWarnings("static-method")
	void setPermissionRoundtrip() {
		DocumentPrivilege p = new DocumentPrivilege();
		p.setPermission(DocumentPermission.CRUDC);
		assertThat(p.getPermission(), is(DocumentPermission.CRUDC));
	}

	@Test
	@SuppressWarnings("static-method")
	void permissionNullByDefault() {
		DocumentPrivilege p = new DocumentPrivilege();
		assertNull(p.getPermission());
	}
}
