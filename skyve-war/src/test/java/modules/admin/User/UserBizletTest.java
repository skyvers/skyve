package modules.admin.User;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

import modules.admin.domain.AdminDocument;

class UserBizletTest extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void testPreSaveSetsPasswordExpired() throws Exception {
		// setup the test data
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(AdminDocument.USER);
		user.setPasswordExpired(Boolean.FALSE);
		user.setGeneratedPassword("test");

		// validate the test data
		assertThat(user.getGeneratedPassword(), is(notNullValue()));

		// call the method under test
		new UserBizlet().preSave(user);

		// verify the result
		assertThat(user.getPasswordExpired(), is(Boolean.TRUE));
	}

}
