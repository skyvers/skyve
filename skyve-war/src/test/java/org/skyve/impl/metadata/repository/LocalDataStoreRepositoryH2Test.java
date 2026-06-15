package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Startup.StartupBizlet;
import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.User;
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
