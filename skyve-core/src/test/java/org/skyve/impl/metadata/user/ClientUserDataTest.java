package org.skyve.impl.metadata.user;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class ClientUserDataTest {

	@Test
	@SuppressWarnings("static-method")
	public void setIdRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setId("user1");
		assertThat(data.getId(), is("user1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setNameRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setName("Alice");
		assertThat(data.getName(), is("Alice"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setContactIdRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setContactId("c1");
		assertThat(data.getContactId(), is("c1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setContactNameRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setContactName("Bob");
		assertThat(data.getContactName(), is("Bob"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setCustomerNameRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setCustomerName("acme");
		assertThat(data.getCustomerName(), is("acme"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDataGroupIdRoundtrip() {
		ClientUserData data = new ClientUserData();
		data.setDataGroupId("dg1");
		assertThat(data.getDataGroupId(), is("dg1"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void actionsNullByDefault() {
		ClientUserData data = new ClientUserData();
		assertNull(data.getActions());
	}

	@Test
	@SuppressWarnings("static-method")
	public void roleNamesNullByDefault() {
		ClientUserData data = new ClientUserData();
		assertNull(data.getRoleNames());
	}

	@Test
	@SuppressWarnings("static-method")
	public void moduleMenuMapNotNullByDefault() {
		ClientUserData data = new ClientUserData();
		assertNotNull(data.getModuleMenuMap());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setRoleNamesRoundtrip() {
		ClientUserData data = new ClientUserData();
		java.util.Set<String> roles = new java.util.HashSet<>();
		roles.add("admin");
		data.setRoleNames(roles);
		assertThat(data.getRoleNames(), is(roles));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setActionsRoundtrip() {
		ClientUserData data = new ClientUserData();
		java.util.Set<String> actions = new java.util.HashSet<>();
		actions.add("save");
		data.setActions(actions);
		assertThat(data.getActions(), is(actions));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setDocumentPermissionsRoundtrip() {
		ClientUserData data = new ClientUserData();
		java.util.Map<String, java.util.Map<String, Boolean>> perms = new java.util.TreeMap<>();
		data.setDocumentPermissions(perms);
		assertThat(data.getDocumentPermissions(), is(perms));
	}

	@Test
	@SuppressWarnings("static-method")
	public void setModuleMenuMapRoundtrip() {
		ClientUserData data = new ClientUserData();
		java.util.Map<String, org.skyve.metadata.module.menu.Menu> map = new java.util.TreeMap<>();
		data.setModuleMenuMap(map);
		assertThat(data.getModuleMenuMap(), is(map));
	}
}
