package modules.admin.User.actions;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.User;
import util.AbstractH2Test;

public class GeneratePasswordActionTest extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	public void testExecuteGeneratesNewPassword() throws Exception {
		// setup the test data
		User user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setConfirmPassword(null);
		user.setGeneratedPassword(null);
		user.setPasswordExpired(Boolean.FALSE);

		// call the method under test
		new GeneratePassword().execute(user, null);

		// verify the result
		assertThat(user.getPasswordExpired(), is(Boolean.TRUE));
		assertThat(user.getGeneratedPassword(), is(notNullValue()));
		assertThat(user.getConfirmPassword(), is(notNullValue()));
	}
}
