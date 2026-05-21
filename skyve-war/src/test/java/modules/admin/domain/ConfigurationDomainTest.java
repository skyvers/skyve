package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Configuration.PasswordComplexityModel;
import modules.admin.domain.Configuration.TwoFactorType;
import util.AbstractH2Test;

/**
 * Tests for the {@link Configuration} admin domain bean (persistent).
 * Supplements existing {@code ConfigurationBizletTest} by exercising more
 * getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class ConfigurationDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesConfigurationBean() throws Exception {
		Configuration bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		Configuration bean = Configuration.newInstance();
		assertEquals(Configuration.MODULE_NAME, bean.getBizModule());
		assertEquals(Configuration.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void twoFactorTypeSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorType(TwoFactorType.off);
		assertEquals(TwoFactorType.off, bean.getTwoFactorType());
		bean.setTwoFactorType(TwoFactorType.email);
		assertEquals(TwoFactorType.email, bean.getTwoFactorType());
	}

	@Test
	void passwordComplexityModelSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		PasswordComplexityModel model = PasswordComplexityModel.values()[0];
		bean.setPasswordComplexityModel(model);
		assertEquals(model, bean.getPasswordComplexityModel());
	}

	@Test
	void fromEmailAndSubjectSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setFromEmail("admin@example.com");
		bean.setPasswordResetEmailSubject("Reset your password");
		assertEquals("admin@example.com", bean.getFromEmail());
		assertEquals("Reset your password", bean.getPasswordResetEmailSubject());
	}

	@Test
	void twoFactorEmailSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorEmailSubject("Your 2FA code");
		bean.setTwoFactorEmailBody("<p>Your code is {code}</p>");
		assertEquals("Your 2FA code", bean.getTwoFactorEmailSubject());
		assertEquals("<p>Your code is {code}</p>", bean.getTwoFactorEmailBody());
	}

	@Test
	void emailNotificationFieldsSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setEmailFrom("noreply@example.com");
		bean.setEmailTo("admin@example.com");
		bean.setEmailSubject("Alert");
		bean.setEmailContent("<p>Content</p>");
		assertEquals("noreply@example.com", bean.getEmailFrom());
		assertEquals("admin@example.com", bean.getEmailTo());
		assertEquals("Alert", bean.getEmailSubject());
		assertEquals("<p>Content</p>", bean.getEmailContent());
	}

	@Test
	void passwordPolicyFieldsSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordExpiryDays("90");
		bean.setPasswordHistoryRetention("5");
		bean.setPasswordAccountLockoutThreshold("5");
		bean.setPasswordAccountLockoutDuration("30");
		assertEquals("90", bean.getPasswordExpiryDays());
		assertEquals("5", bean.getPasswordHistoryRetention());
		assertEquals("5", bean.getPasswordAccountLockoutThreshold());
		assertEquals("30", bean.getPasswordAccountLockoutDuration());
	}

	@Test
	void allowUserSelfRegistrationSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setAllowUserSelfRegistration(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getAllowUserSelfRegistration());
	}

	@Test
	void selfRegistrationExpiryHoursSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setSelfRegistrationActivationExpiryHours(Integer.valueOf(48));
		assertEquals(Integer.valueOf(48), bean.getSelfRegistrationActivationExpiryHours());
	}

	@Test
	void availableDiskSpaceSetAndGet() throws Exception {
		Configuration bean = Configuration.newInstance();
		bean.setAvailableDiskSpaceAlarmLevelPercentage(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getAvailableDiskSpaceAlarmLevelPercentage());
	}
}
