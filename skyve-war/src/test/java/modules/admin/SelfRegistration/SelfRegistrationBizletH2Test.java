package modules.admin.SelfRegistration;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Contact;
import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SelfRegistrationBizletH2Test extends AbstractH2Test {
	@Test
	void newInstanceSeedsUserDocument() throws Exception {
		SelfRegistrationExtension bean = new SelfRegistrationExtension();

		SelfRegistrationExtension result = new SelfRegistrationBizlet().newInstance(bean);

		assertThat(result, is(bean));
		assertThat(bean.getUser(), is(notNullValue()));
	}

	@Test
	void validateAddsResendActivationMessageWhenEmailBelongsToInactiveUser() throws Exception {
		UserExtension existing = savedUser("inactive-registration", Boolean.FALSE);
		SelfRegistrationExtension bean = registration(existing.getUserName());
		ValidationException validation = new ValidationException();

		new SelfRegistrationBizlet().validate(bean, validation);

		assertThat(validation.getMessages().get(0).getText(), containsString("resend your activation email"));
		assertThat(validation.getMessages().get(0).getText(), containsString(existing.getBizId()));
	}

	@Test
	void validateAddsPasswordResetMessageWhenEmailBelongsToActivatedUser() throws Exception {
		UserExtension existing = savedUser("active-registration", Boolean.TRUE);
		SelfRegistrationExtension bean = registration(existing.getUserName());
		ValidationException validation = new ValidationException();

		new SelfRegistrationBizlet().validate(bean, validation);

		assertThat(validation.getMessages().get(0).getText(), containsString("reset your password"));
	}

	private static SelfRegistrationExtension registration(String email) throws Exception {
		SelfRegistrationExtension bean = new SelfRegistrationBizlet().newInstance(new SelfRegistrationExtension());
		bean.getUser().setContact(Contact.newInstance());
		bean.getUser().getContact().setEmail1(email);
		return bean;
	}

	private static UserExtension savedUser(String prefix, Boolean activated) {
		String unique = prefix + "-" + UUID.randomUUID();
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName(unique + "@example.com");
		user.getContact().setEmail1(unique + "@example.com");
		user.getContact().setName(unique);
		user.setActivated(activated);
		return CORE.getPersistence().save(user);
	}
}
