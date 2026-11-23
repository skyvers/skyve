package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.AdminDocument;
import util.AbstractH2Test;

class UserExtensionTest extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void testToUserProxy() throws Exception {
		// setup test data
		UserExtension ue = new DataBuilder().fixture(FixtureType.crud).build(AdminDocument.USER);

		// persist user
		ue = CORE.getPersistence().save(ue);

		// convert user to user proxy
		UserProxyExtension upe = ue.toUserProxy();

		// validate User Proxy
		assertThat(upe, is(notNullValue()));

		// convert user proxy to user
		UserExtension ue2 = upe.toUser();

		// validate test data
		assertThat(ue2, is(notNullValue()));

		// check user returned is original user
		assertThat(ue2, is(ue));

		// Delete persisted user
		CORE.getPersistence().delete(ue);
	}
}
