package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * What the user a session is built from looks like once the database says a user is disabled, must change
 * their password, or has had roles, groups or a data group changed: the repository reads the database on
 * each retrieval, so a change takes effect for the next session (a session already running keeps its user
 * until it ends, which {@code UserBizlet} arranges for disabling).
 */
@SuppressWarnings({ "static-method", "boxing" })
class UserPrivilegesH2Test extends AbstractH2Test {
	private int expiryDays;

	@BeforeEach
	void rememberExpiry() {
		expiryDays = UtilImpl.PASSWORD_EXPIRY_IN_DAYS;
	}

	@AfterEach
	void restoreExpiry() {
		UtilImpl.PASSWORD_EXPIRY_IN_DAYS = expiryDays;
	}

	@Test
	void anInactiveUserIsRefusedAccessToTheSystem() {
		String name = name("inactive");
		UserExtension user = user(name);
		user.setInactive(Boolean.TRUE);
		addRole(user, "admin.BasicUser");
		saveAndCommit(user);

		assertThrows(SecurityException.class, () -> retrieve(name));
	}

	@Test
	void anUnknownUserIsRefusedAccessToTheSystem() {
		String ghost = name("ghost");

		assertThrows(SecurityException.class, () -> retrieve(ghost));
	}

	@Test
	void removingARoleTakesAwayItsPrivilegesForTheNextRetrieval() {
		String name = name("revoke");
		UserExtension user = user(name);
		addRole(user, "admin.BasicUser");
		addRole(user, "admin.SecurityAdministrator");
		user = saveAndCommit(user);
		assertTrue(retrieve(name).isInRole("admin", "SecurityAdministrator"));

		user.getRoles().removeIf(r -> "admin.SecurityAdministrator".equals(r.getRoleName()));
		saveAndCommit(user);

		UserImpl after = retrieve(name);
		assertFalse(after.isInRole("admin", "SecurityAdministrator"));
		assertTrue(after.isInRole("admin", "BasicUser"));
	}

	@Test
	void aGroupGivesItsRolesAndLeavingTheGroupTakesThemAway() {
		String name = name("group");
		GroupExtension group = new DataBuilder().fixture(FixtureType.crud).build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("group-" + System.nanoTime());
		group.getRoles().clear();
		GroupRole groupRole = GroupRole.newInstance();
		groupRole.setRoleName("admin.SecurityAdministrator");
		group.addRolesElement(groupRole);
		group = CORE.getPersistence().save(group);
		UserExtension user = user(name);
		addRole(user, "admin.BasicUser");
		user.addGroupsElement(group);
		user = saveAndCommit(user);
		assertTrue(retrieve(name).isInRole("admin", "SecurityAdministrator"));

		user.getGroups().clear();
		saveAndCommit(user);

		assertFalse(retrieve(name).isInRole("admin", "SecurityAdministrator"));
	}

	@Test
	void aGroupAloneIsEnoughToSignIn() {
		String name = name("grouponly");
		GroupExtension group = new DataBuilder().fixture(FixtureType.crud).build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("grouponly-" + System.nanoTime());
		group.getRoles().clear();
		GroupRole groupRole = GroupRole.newInstance();
		groupRole.setRoleName("admin.BasicUser");
		group.addRolesElement(groupRole);
		group = CORE.getPersistence().save(group);
		UserExtension user = user(name);
		user.addGroupsElement(group);
		saveAndCommit(user);

		assertTrue(retrieve(name).isInRole("admin", "BasicUser"));
	}

	@Test
	void aPasswordExpiredUserMustChangeTheirPassword() {
		String name = name("expired");
		UserExtension user = user(name);
		user.setPasswordExpired(Boolean.TRUE);
		addRole(user, "admin.BasicUser");
		saveAndCommit(user);

		assertTrue(retrieve(name).isPasswordChangeRequired());
	}

	@Test
	void aUserWhosePasswordHasNotExpiredIsNotMadeToChangeIt() {
		String name = name("fine");
		UserExtension user = user(name);
		user.setPasswordExpired(Boolean.FALSE);
		addRole(user, "admin.BasicUser");
		saveAndCommit(user);

		assertFalse(retrieve(name).isPasswordChangeRequired());
	}

	@Test
	void aPasswordOlderThanTheExpiryDaysMustBeChangedAndANewerOneNeedNotBe() throws Exception {
		UtilImpl.PASSWORD_EXPIRY_IN_DAYS = 30;
		String old = name("old");
		String recent = name("recent");
		String never = name("never");
		for (String name : new String[] { old, recent, never }) {
			UserExtension user = user(name);
			user.setPasswordExpired(Boolean.FALSE);
			addRole(user, "admin.BasicUser");
			saveAndCommit(user);
		}
		setPasswordLastChanged(old, System.currentTimeMillis() - (31L * 86_400_000L));
		setPasswordLastChanged(recent, System.currentTimeMillis() - (29L * 86_400_000L));
		setPasswordLastChanged(never, null);

		assertTrue(retrieve(old).isPasswordChangeRequired());
		assertFalse(retrieve(recent).isPasswordChangeRequired());
		assertTrue(retrieve(never).isPasswordChangeRequired(), "a password with no recorded change date counts as expired");
	}

	@Test
	void withNoExpiryDaysAnOldPasswordIsLeftAlone() throws Exception {
		UtilImpl.PASSWORD_EXPIRY_IN_DAYS = 0;
		String name = name("noexpiry");
		UserExtension user = user(name);
		user.setPasswordExpired(Boolean.FALSE);
		addRole(user, "admin.BasicUser");
		saveAndCommit(user);
		setPasswordLastChanged(name, System.currentTimeMillis() - (3650L * 86_400_000L));

		assertFalse(retrieve(name).isPasswordChangeRequired());
	}

	@Test
	void theUsersDataGroupScopesTheirData() {
		String name = name("datagroup");
		DataGroup dataGroup = new DataBuilder().fixture(FixtureType.crud).build(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
		dataGroup = CORE.getPersistence().save(dataGroup);
		UserExtension user = user(name);
		user.setDataGroup(dataGroup);
		addRole(user, "admin.BasicUser");
		saveAndCommit(user);

		assertEquals(dataGroup.getBizId(), retrieve(name).getDataGroupId());
		assertNotNull(retrieve(name).getId());
	}

	private static UserImpl retrieve(String name) {
		return new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + name);
	}

	private static String name(String prefix) {
		return "priv." + prefix + "." + System.nanoTime();
	}

	private static UserExtension user(String name) {
		UserExtension result = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		result.setUserName(name);
		result.setInactive(Boolean.FALSE);
		result.setPasswordExpired(Boolean.FALSE);
		result.setDataGroup(null);
		result.getGroups().clear();
		result.getRoles().clear();
		return result;
	}

	private static void addRole(UserExtension user, String roleName) {
		UserRole role = UserRole.newInstance();
		role.setRoleName(roleName);
		user.addRolesElement(role);
	}

	private static UserExtension saveAndCommit(UserExtension user) {
		UserExtension result = CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return result;
	}

	private static void setPasswordLastChanged(String name, Long millis) throws Exception {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement ps = c.prepareStatement(
						"update ADM_SecurityUser set passwordLastChanged = ? where bizCustomer = ? and userName = ?")) {
			c.setAutoCommit(true);
			ps.setTimestamp(1, (millis == null) ? null : new Timestamp(millis.longValue()));
			ps.setString(2, CUSTOMER);
			ps.setString(3, name);
			assertEquals(1, ps.executeUpdate());
		}
	}
}
