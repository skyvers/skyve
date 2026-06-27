package modules.admin.SelfRegistrationActivation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.SelfRegistrationActivation.Result;
import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SelfRegistrationActivationExtensionH2Test extends AbstractH2Test {
	@Test
	void activateUserWithUnknownCodeSetsFailureResult() {
		SelfRegistrationActivationExtension activation = new SelfRegistrationActivationExtension();

		UserExtension result = activation.activateUser("missing-" + UUID.randomUUID());

		assertNull(result);
		assertThat(activation.getResult(), is(Result.FAILURE));
	}

	@Test
	void activateUserWithAlreadyActivatedUserSetsAlreadyActivatedResult() {
		UserExtension user = savedUser("already-activated", Boolean.TRUE);
		SelfRegistrationActivationExtension activation = new SelfRegistrationActivationExtension();

		UserExtension result = activation.activateUser(user.getActivationCode());

		assertThat(result, is(notNullValue()));
		assertThat(result.getBizId(), is(user.getBizId()));
		assertThat(activation.getUser().getBizId(), is(user.getBizId()));
		assertThat(activation.getResult(), is(Result.ALREADYACTIVATED));
	}

	@Test
	void activateUserWithFreshCodeActivatesUser() {
		UserExtension user = savedUser("fresh-activation", Boolean.FALSE);
		SelfRegistrationActivationExtension activation = new SelfRegistrationActivationExtension();

		UserExtension result = activation.activateUser(user.getActivationCode());

		assertThat(result, is(notNullValue()));
		assertThat(result.getActivated(), is(Boolean.TRUE));
		assertThat(activation.getUser().getBizId(), is(result.getBizId()));
		assertThat(activation.getResult(), is(Result.SUCCESS));
	}

	private static UserExtension savedUser(String prefix, Boolean activated) {
		String unique = prefix + "-" + UUID.randomUUID();
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName(unique + "@example.com");
		user.getContact().setName(unique);
		user.getContact().setEmail1(unique + "@example.com");
		user.setActivationCode(UUID.randomUUID().toString());
		user.setActivationCodeCreationDateTime(new DateTime());
		user.setActivated(activated);
		return CORE.getPersistence().save(user);
	}
}
