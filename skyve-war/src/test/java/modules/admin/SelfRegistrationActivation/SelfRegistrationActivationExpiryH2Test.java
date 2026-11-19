package modules.admin.SelfRegistrationActivation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Startup.StartupBizlet;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.SelfRegistrationActivation.Result;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * Activating a self-registered account from the emailed link: the link works inside its expiry, not after it,
 * works for any age when no expiry is configured, and works once.
 */
@SuppressWarnings("static-method")
class SelfRegistrationActivationExpiryH2Test extends AbstractH2Test {
	@AfterEach
	void clearExpiry() {
		setExpiryHours(null);
	}

	@Test
	void aLinkInsideItsExpiryActivatesTheAccount() {
		setExpiryHours(Integer.valueOf(24));
		UserExtension user = registered(hoursAgo(1));

		SelfRegistrationActivationExtension activation = activate(user);

		assertEquals(Result.SUCCESS, activation.getResult());
		assertTrue(activated(user));
	}

	@Test
	void aLinkPastItsExpiryDoesNotActivateTheAccount() {
		setExpiryHours(Integer.valueOf(24));
		UserExtension user = registered(hoursAgo(25));

		SelfRegistrationActivationExtension activation = activate(user);

		assertEquals(Result.EXPIRED, activation.getResult());
		assertFalse(activated(user));
	}

	@Test
	void withNoExpiryConfiguredAnOldLinkStillWorks() {
		setExpiryHours(null);
		UserExtension user = registered(hoursAgo(24 * 365));

		assertEquals(Result.SUCCESS, activate(user).getResult());
	}

	@Test
	void aLinkWorksOnceAndThenOnlySaysTheAccountIsAlreadyActivated() {
		setExpiryHours(Integer.valueOf(24));
		UserExtension user = registered(hoursAgo(1));
		assertEquals(Result.SUCCESS, activate(user).getResult());

		assertEquals(Result.ALREADYACTIVATED, activate(user).getResult());
	}

	private static SelfRegistrationActivationExtension activate(UserExtension user) {
		SelfRegistrationActivationExtension result = new SelfRegistrationActivationExtension();
		result.activateUser(user.getActivationCode());
		return result;
	}

	private static boolean activated(UserExtension user) {
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return Boolean.TRUE.equals(CORE.getPersistence()
				.newSQL("select activated from ADM_SecurityUser where bizId = :id")
				.putParameter("id", user.getBizId(), false)
				.scalarResult(Boolean.class));
	}

	private static DateTime hoursAgo(int hours) {
		return new DateTime(System.currentTimeMillis() - (hours * 3_600_000L));
	}

	@SuppressWarnings("null") // a null here fails the test
	private static UserExtension registered(DateTime codeCreated) {
		String unique = "expiry-" + UUID.randomUUID();
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName(unique + "@example.com");
		user.getContact().setName(unique);
		user.getContact().setEmail1(unique + "@example.com");
		user.setActivationCode(UUID.randomUUID().toString());
		user.setActivationCodeCreationDateTime(codeCreated);
		user.setActivated(Boolean.FALSE);
		UserExtension result = CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return result;
	}

	/**
	 * Sets the activation expiry on the (singleton) configuration through persistence, so its caches see it.
	 */
	private static void setExpiryHours(Integer hours) {
		ConfigurationExtension configuration = Configuration.newInstance();
		if (!configuration.isPersisted()) {
			configuration = new DataBuilder().fixture(FixtureType.crud)
					.build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		}
		configuration.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_GMAP);
		configuration.getStartup().setMailPort(Integer.valueOf(25));
		if (configuration.getPasswordMinLength() == null) {
			configuration.setPasswordMinLength(Integer.valueOf(ConfigurationExtension.PASSWORD_DEFAULT_MIN_LENGTH));
		}
		configuration.setSelfRegistrationActivationExpiryHours(hours);
		CORE.getPersistence().save(configuration);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
	}
}
