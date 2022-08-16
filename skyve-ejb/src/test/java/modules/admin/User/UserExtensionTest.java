package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.CORE;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

public class UserExtensionTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("deprecation")
	public void testToUserProxy() throws Exception {
		// setup test data
		UserExtension ue = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);

		// persist user
		ue = CORE.getPersistence().save(ue);

		// convert user to user proxy
		UserProxyExtension upe = ue.toUserProxy();

		// validate test data
		assertThat(upe, is(notNullValue()));
		assertTrue(upe instanceof UserProxyExtension);

		// Delete persisted user
		CORE.getPersistence().delete(ue);
	}

}
