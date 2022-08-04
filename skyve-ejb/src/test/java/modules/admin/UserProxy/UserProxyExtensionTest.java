package modules.admin.UserProxy;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;

public class UserProxyExtensionTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("deprecation")
	public void testUserFromUserProxy() throws Exception {
		// setup test data
		UserProxyExtension upe = new DataBuilder().fixture(FixtureType.crud).build(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME);
		
		// convert user to user proxy
		UserExtension ue = upe.userFromUserProxy();
		
		// validate test data
		assertThat(ue, is(notNullValue()));
		assertThat(ue, is(UserExtension.class));
		
		// convert user proxy back to user
		UserProxyExtension upe2 = ue.userProxyFromUser();

		// validate test data
		assertThat(upe2, is(notNullValue()));
		assertThat(upe2, is(UserProxyExtension.class));

		// validate user to proxy and back to user is the same object
		assertThat(upe2, is(upe));
	}

}
