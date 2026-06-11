package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.AbstractSkyveTest;

class UserDashboardDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesUserDashboard() {
		UserDashboard bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(UserDashboard.MODULE_NAME, UserDashboard.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesUserDashboard() {
		UserDashboard bean = UserDashboard.newInstance();
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() {
		UserDashboard bean = UserDashboard.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("UserDashboard", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void favouritesListIsNotNull() {
		UserDashboard bean = UserDashboard.newInstance();
		assertNotNull(bean.getFavourites());
	}
}
