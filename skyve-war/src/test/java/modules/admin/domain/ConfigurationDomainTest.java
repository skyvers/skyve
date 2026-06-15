package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
	void dataBuilderPopulatesConfigurationBean() {
		Configuration bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		Configuration bean = Configuration.newInstance();
		assertEquals(Configuration.MODULE_NAME, bean.getBizModule());
		assertEquals(Configuration.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void twoFactorTypeSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorType(TwoFactorType.off);
		assertEquals(TwoFactorType.off, bean.getTwoFactorType());
		bean.setTwoFactorType(TwoFactorType.email);
		assertEquals(TwoFactorType.email, bean.getTwoFactorType());
	}

	@Test
	@SuppressWarnings("deprecation")
	void passwordComplexityModelSetAndGet() {
		Configuration bean = Configuration.newInstance();
		PasswordComplexityModel model = PasswordComplexityModel.values()[0];
		bean.setPasswordComplexityModel(model);
		assertEquals(model, bean.getPasswordComplexityModel());
	}

	@Test
	void fromEmailAndSubjectSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setFromEmail("admin@example.com");
		bean.setPasswordResetEmailSubject("Reset your password");
		assertEquals("admin@example.com", bean.getFromEmail());
		assertEquals("Reset your password", bean.getPasswordResetEmailSubject());
	}

	@Test
	void twoFactorEmailSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorEmailSubject("Your 2FA code");
		bean.setTwoFactorEmailBody("<p>Your code is {code}</p>");
		assertEquals("Your 2FA code", bean.getTwoFactorEmailSubject());
		assertEquals("<p>Your code is {code}</p>", bean.getTwoFactorEmailBody());
	}

	@Test
	void emailNotificationFieldsSetAndGet() {
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
	void passwordPolicyFieldsSetAndGet() {
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
	@SuppressWarnings("deprecation")
	void allowUserSelfRegistrationSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setAllowUserSelfRegistration(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getAllowUserSelfRegistration());
	}

	@Test
	void selfRegistrationExpiryHoursSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setSelfRegistrationActivationExpiryHours(Integer.valueOf(48));
		assertEquals(Integer.valueOf(48), bean.getSelfRegistrationActivationExpiryHours());
	}

	@Test
	void availableDiskSpaceSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setAvailableDiskSpaceAlarmLevelPercentage(Integer.valueOf(10));
		assertEquals(Integer.valueOf(10), bean.getAvailableDiskSpaceAlarmLevelPercentage());
	}

	// === PasswordComplexityModel enum tests ===

	@Test
	void passwordComplexityModelFromCode() {
		assertEquals(PasswordComplexityModel.minimumMin6Chars, PasswordComplexityModel.fromCode("MINIMUM"));
		assertEquals(PasswordComplexityModel.mediumMin6CharsUpperLowerAndNumeric, PasswordComplexityModel.fromCode("MEDIUM"));
		assertNull(PasswordComplexityModel.fromCode("unknown"));
	}

	@Test
	void passwordComplexityModelToCode() {
		assertEquals("MINIMUM", PasswordComplexityModel.minimumMin6Chars.toCode());
		assertEquals("MAXIMUM", PasswordComplexityModel.goodMin8CharsUpperLowerNumericAndPunctuation.toCode());
		assertEquals("STRONG", PasswordComplexityModel.strongMin10CharsUpperLowerNumericAndPunctuation.toCode());
	}

	@Test
	void passwordComplexityModelToDomainValues() {
		assertNotNull(PasswordComplexityModel.toDomainValues());
		assertFalse(PasswordComplexityModel.toDomainValues().isEmpty());
	}

	@Test
	void passwordComplexityModelToDomainValue() {
		assertNotNull(PasswordComplexityModel.minimumMin6Chars.toDomainValue());
	}

	@Test
	void passwordComplexityModelToLocalisedDescription() {
		assertNotNull(PasswordComplexityModel.minimumMin6Chars.toLocalisedDescription());
	}

	@Test
	void passwordComplexityModelFromLocalisedDescription() {
		String desc = PasswordComplexityModel.minimumMin6Chars.toLocalisedDescription();
		assertEquals(PasswordComplexityModel.minimumMin6Chars, PasswordComplexityModel.fromLocalisedDescription(desc));
	}

	@Test
	void passwordComplexityModelFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(PasswordComplexityModel.fromLocalisedDescription("nonexistent description xyz"));
	}

	// === TwoFactorType enum tests ===

	@Test
	void twoFactorTypeFromCode() {
		assertEquals(TwoFactorType.off, TwoFactorType.fromCode("OFF"));
		assertEquals(TwoFactorType.email, TwoFactorType.fromCode("EMAIL"));
		assertNull(TwoFactorType.fromCode("unknown"));
	}

	@Test
	void twoFactorTypeToCode() {
		assertEquals("OFF", TwoFactorType.off.toCode());
		assertEquals("EMAIL", TwoFactorType.email.toCode());
	}

	@Test
	void twoFactorTypeToDomainValues() {
		assertNotNull(TwoFactorType.toDomainValues());
		assertFalse(TwoFactorType.toDomainValues().isEmpty());
	}

	@Test
	void twoFactorTypeToDomainValue() {
		assertNotNull(TwoFactorType.off.toDomainValue());
	}

	@Test
	void twoFactorTypeToLocalisedDescription() {
		assertNotNull(TwoFactorType.off.toLocalisedDescription());
	}

	@Test
	void twoFactorTypeFromLocalisedDescription() {
		String desc = TwoFactorType.off.toLocalisedDescription();
		assertEquals(TwoFactorType.off, TwoFactorType.fromLocalisedDescription(desc));
	}

	@Test
	void twoFactorTypeFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(TwoFactorType.fromLocalisedDescription("nonexistent type xyz"));
	}

	// === Boolean derived methods ===

	@Test
	void isTfaEmailSelectedWhenEmailSet() {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorType(TwoFactorType.email);
		assertTrue(bean.isTfaEmailSelected());
		assertFalse(bean.isNotTfaEmailSelected());
	}

	@Test
	void isNotTfaEmailSelectedWhenOff() {
		Configuration bean = Configuration.newInstance();
		bean.setTwoFactorType(TwoFactorType.off);
		assertFalse(bean.isTfaEmailSelected());
		assertTrue(bean.isNotTfaEmailSelected());
	}

	@Test
	void passwordMinLengthSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordMinLength(Integer.valueOf(8));
		assertEquals(Integer.valueOf(8), bean.getPasswordMinLength());
	}

	@Test
	void passwordRequireLowercaseSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordRequireLowercase(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getPasswordRequireLowercase());
	}

	@Test
	void passwordRequireUppercaseSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordRequireUppercase(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getPasswordRequireUppercase());
	}

	@Test
	void passwordRequireNumericSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordRequireNumeric(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getPasswordRequireNumeric());
	}

	@Test
	void passwordRequireSpecialSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordRequireSpecial(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getPasswordRequireSpecial());
	}

	@Test
	void passwordResetTokenExpiryMinutesSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setPasswordResetTokenExpiryMinutes(Integer.valueOf(30));
		assertEquals(Integer.valueOf(30), bean.getPasswordResetTokenExpiryMinutes());
	}

	@Test
	void availableDiskSpaceAlarmLevelMBSetAndGet() {
		Configuration bean = Configuration.newInstance();
		bean.setAvailableDiskSpaceAlarmLevelMB(Long.valueOf(1024L));
		assertEquals(Long.valueOf(1024L), bean.getAvailableDiskSpaceAlarmLevelMB());
	}

	@Test
	void isAvailableDiskSpaceAlarmConfiguredConditions() {
			Configuration bean = Configuration.newInstance();
			// Without level set
			assertFalse(bean.isAvailableDiskSpaceAlarmConfigured());
			assertTrue(bean.isNotAvailableDiskSpaceAlarmConfigured());
	}

	@Test
	void isEmailConfiguredConditions() {
			Configuration bean = Configuration.newInstance();
			assertFalse(bean.isEmailConfigured());
			assertTrue(bean.isNotEmailConfigured());
	}

	@Test
	void isBackupTypeAzureConditions() {
			Configuration bean = Configuration.newInstance();
			assertFalse(bean.isBackupTypeAzure());
			assertTrue(bean.isNotBackupTypeAzure());
	}

	@Test
	void isMapTypeGmapConditions() {
			Configuration bean = Configuration.newInstance();
			// Without map type set
			assertFalse(bean.isMapTypeGmap());
			assertTrue(bean.isNotMapTypeGmap());
	}

	@Test
	void isSingleTenantConditions() {
			Configuration bean = Configuration.newInstance();
			assertFalse(bean.isSingleTenant());
			assertTrue(bean.isNotSingleTenant());
	}

	@Test
	@SuppressWarnings("boxing")
	void isIpAddressChecksEnabledConditions() {
		Configuration bean = Configuration.newInstance();
		assertEquals(! bean.isIpAddressChecksEnabled(), bean.isNotIpAddressChecksEnabled());
	}

	@Test
	void isBackupsConfiguredConditions() {
		Configuration bean = Configuration.newInstance();
		assertFalse(bean.isBackupsConfigured());
		assertTrue(bean.isNotBackupsConfigured());
	}
}
