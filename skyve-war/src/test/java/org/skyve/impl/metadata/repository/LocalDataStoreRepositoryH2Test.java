package org.skyve.impl.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class LocalDataStoreRepositoryH2Test extends AbstractH2Test {
	@Test
	void retrievePublicUserNameReturnsNullWhenNoPublicUserConfigured() {
		ConfigurationExtension configuration = new DataBuilder().fixture(FixtureType.crud).build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		configuration.setPublicUser(null);
		configuration = CORE.getPersistence().save(configuration);

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

		ConfigurationExtension configuration = new DataBuilder().fixture(FixtureType.crud).build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		configuration.setPublicUser(publicUser);
		CORE.getPersistence().save(configuration);

		LocalDataStoreRepository repository = new LocalDataStoreRepository();

		assertEquals(expectedUserName, repository.retrievePublicUserName(CUSTOMER));
	}
}
