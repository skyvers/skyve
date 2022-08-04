package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

public class UserExtensionTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("deprecation")
	public void testUserProxyFromUser() throws Exception {
		// setup test data
		UserExtension ue = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		
		// convert user to user proxy
		UserProxyExtension upe = ue.userProxyFromUser();
		
		// validate test data
		assertThat(upe, is(notNullValue()));
		assertThat(upe, is(UserProxyExtension.class));
		
		// convert user proxy back to user
		UserExtension ue2 = upe.userFromUserProxy();

		// validate test data
		assertThat(ue2, is(notNullValue()));
		assertThat(ue2, is(UserExtension.class));

		// validate user to proxy and back to user is the same object
		assertThat(ue2, is(ue));
	}

}
