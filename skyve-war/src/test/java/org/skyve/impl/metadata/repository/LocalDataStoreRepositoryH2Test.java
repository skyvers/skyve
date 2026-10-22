package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Startup.StartupBizlet;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.Group;
import modules.admin.domain.GroupRole;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class LocalDataStoreRepositoryH2Test extends AbstractH2Test {
	@Test
	void retrievePublicUserNameReturnsNullWhenNoPublicUserConfigured() {
		ConfigurationExtension configuration = saveConfiguration(null);

		LocalDataStoreRepository repository = new LocalDataStoreRepository();

		assertNull(repository.retrievePublicUserName(CUSTOMER));
		assertNotNull(configuration.getBizId());
	}

	@Test
	void retrievePublicUserNameReturnsConfiguredPublicUserName() {
		String expectedUserName = "public." + System.nanoTime();
		UserExtension publicUser = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		publicUser.setUserName(expectedUserName);
		publicUser = CORE.getPersistence().save(publicUser);
		saveConfiguration(publicUser);

		LocalDataStoreRepository repository = new LocalDataStoreRepository();

		assertEquals(expectedUserName, repository.retrievePublicUserName(CUSTOMER));
	}

	@Test
	void retrieveUserLoadsLegacyIdForUserWithDirectRole() {
		String userName = "legacy.role." + System.nanoTime();
		UserExtension user = user(userName, "BHM-42");
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.BasicUser");
		user.addRolesElement(role);
		saveAndCommit(user);

		UserImpl result = new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + userName);

		assertNotNull(result);
		assertEquals("BHM-42", result.getLegacyId());
	}

	@Test
	void retrieveUserLoadsLegacyIdForUserWithGroupRoleOnly() {
		long suffix = System.nanoTime();
		String userName = "legacy.group." + suffix;
		GroupExtension group = new DataBuilder().fixture(FixtureType.crud).build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("legacy-group-" + suffix);
		group.getRoles().clear();
		GroupRole role = GroupRole.newInstance();
		role.setRoleName("admin.BasicUser");
		group.addRolesElement(role);
		group = CORE.getPersistence().save(group);
		UserExtension user = user(userName, "BHM-43");
		user.addGroupsElement(group);
		saveAndCommit(user);

		UserImpl result = new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + userName);

		assertNotNull(result);
		assertEquals("BHM-43", result.getLegacyId());
	}

	@Test
	void retrieveUserLoadsNullLegacyIdWhenUserHasNone() {
		String userName = "legacy.none." + System.nanoTime();
		UserExtension user = user(userName, null);
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.BasicUser");
		user.addRolesElement(role);
		saveAndCommit(user);

		UserImpl result = new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + userName);

		assertNotNull(result);
		assertNull(result.getLegacyId());
	}

	private static UserExtension user(String userName, String legacyId) {
		UserExtension result = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		result.setUserName(userName);
		result.setInactive(Boolean.FALSE);
		result.setLegacyId(legacyId);
		result.getGroups().clear();
		result.getRoles().clear();
		return result;
	}

	private static UserExtension saveAndCommit(UserExtension user) {
		UserExtension result = CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return result;
	}

	private static ConfigurationExtension saveConfiguration(UserExtension publicUser) {
		ConfigurationExtension configuration = new DataBuilder().fixture(FixtureType.crud).build(Configuration.MODULE_NAME,
				Configuration.DOCUMENT_NAME);
		configuration.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_GMAP);
		configuration.getStartup().setMailPort(Integer.valueOf(25));
		configuration.setPublicUser(publicUser);
		configuration = CORE.getPersistence().save(configuration);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return configuration;
	}
}
