package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.mail.CaptureMailService;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.DataBuilder;
import org.skyve.util.MailService;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Startup.StartupBizlet;
import modules.admin.User.RememberMeTokens;
import modules.admin.User.UserExtension;
import modules.admin.domain.Configuration;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * Forgotten password: a reset token works once, expires, is not burnt by a mistyped confirmation, does not
 * work for a disabled user, a newer request replaces an older token, an unknown email address is answered
 * with silence, and a reset ends the user's other Remember me logins.
 */
@SuppressWarnings("static-method")
class PasswordResetTokenH2Test extends AbstractH2Test {
	private static final String OLD = "OldPassword0!";
	private static final String NEW = "NewPassword0!";
	private static final String NEWER = "NewerPassword0!";

	private final CaptureMailService capture = new CaptureMailService();
	private MailService originalMailService;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void setup() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		MailServiceStaticSingleton.set(capture);
	}

	@AfterEach
	void cleanUp() throws Exception {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
		setExpiryMinutes(null);
	}

	@Test
	void aTokenResetsThePasswordOnceAndThenTheLinkIsDead() throws Exception {
		Fixture f = user("once", Boolean.FALSE, new Timestamp());

		assertNull(WebUtil.resetPassword(f.token, NEW, NEW));
		assertTrue(EXT.checkPassword(NEW, passwordHash(f.name)));

		String second = WebUtil.resetPassword(f.token, NEWER, NEWER);

		assertEquals(Util.nullSafeI18n("exception.passwordResetLinkInvalid"), second);
		assertTrue(EXT.checkPassword(NEW, passwordHash(f.name)), "the second use did not change the password");
		assertFalse(EXT.checkPassword(NEWER, passwordHash(f.name)));
	}

	@Test
	void anExpiredTokenIsRefusedAndThePasswordIsUnchanged() throws Exception {
		setExpiryMinutes(Integer.valueOf(1));
		Fixture f = user("expired", Boolean.FALSE, new Timestamp(System.currentTimeMillis() - 120_000L));

		String result = WebUtil.resetPassword(f.token, NEW, NEW);

		assertEquals(Util.nullSafeI18n("exception.passwordResetTokenExpired"), result);
		assertTrue(EXT.checkPassword(OLD, passwordHash(f.name)));
	}

	@Test
	void aTokenInsideItsExpiryStillWorks() throws Exception {
		setExpiryMinutes(Integer.valueOf(60));
		Fixture f = user("inside", Boolean.FALSE, new Timestamp(System.currentTimeMillis() - 120_000L));

		assertNull(WebUtil.resetPassword(f.token, NEW, NEW));
	}

	@Test
	void aMistypedConfirmationDoesNotBurnTheToken() throws Exception {
		Fixture f = user("typo", Boolean.FALSE, new Timestamp());

		String result = WebUtil.resetPassword(f.token, NEW, NEW + "x");

		assertNotNull(result);
		assertTrue(EXT.checkPassword(OLD, passwordHash(f.name)));
		assertNull(WebUtil.resetPassword(f.token, NEW, NEW), "the link still works for a corrected attempt");
	}

	@Test
	void aPasswordThatIsTooSimpleIsRefusedAndTheTokenSurvives() throws Exception {
		Fixture f = user("weak", Boolean.FALSE, new Timestamp());

		String result = WebUtil.resetPassword(f.token, "abc", "abc");

		assertNotNull(result);
		assertTrue(EXT.checkPassword(OLD, passwordHash(f.name)));
		assertEquals(f.token, tokenOf(f.name));
	}

	@Test
	void aDisabledUsersTokenIsAnsweredLikeAnExpiredOneAndThePasswordIsUnchanged() throws Exception {
		Fixture f = user("disabled", Boolean.TRUE, new Timestamp());

		assertEquals(Util.nullSafeI18n("exception.passwordResetTokenExpired"), WebUtil.resetPassword(f.token, NEW, NEW));

		assertTrue(EXT.checkPassword(OLD, passwordHash(f.name)));
	}

	@Test
	void aResetByTokenDoesNotLeaveTheMarkThatLetsALaterChangeSkipTheOldPassword() throws Exception {
		Fixture f = user("mark", Boolean.FALSE, new Timestamp());

		assertNull(WebUtil.resetPassword(f.token, NEW, NEW));

		assertFalse(CORE.getStash().containsKey(AppConstants.PASSWORD_RESET_STASH_KEY));
	}

	@Test
	void aSuccessfulResetEndsTheUsersRememberMeLogins() throws Exception {
		Fixture f = user("remember", Boolean.FALSE, new Timestamp());
		String principal = CUSTOMER + '/' + f.name;
		RememberMeTokens.add(CORE.getPersistence(), CUSTOMER, principal, "phone");
		RememberMeTokens.add(CORE.getPersistence(), CUSTOMER, principal, "pc");
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();

		assertNull(WebUtil.resetPassword(f.token, NEW, NEW));

		CORE.getPersistence().begin();
		assertEquals(List.of(), RememberMeTokens.series(CORE.getPersistence(), principal));
	}

	@Test
	void aNewRequestReplacesTheOlderToken() throws Exception {
		Fixture f = user("replace", Boolean.FALSE, new Timestamp());
		String email = emailOf(f.name);

		WebUtil.requestPasswordReset(CUSTOMER, email);
		String first = tokenOf(f.name);
		WebUtil.requestPasswordReset(CUSTOMER, email);
		String second = tokenOf(f.name);

		assertNotNull(first);
		assertNotNull(second);
		assertNotEquals(first, second);
		assertEquals(Util.nullSafeI18n("exception.passwordResetLinkInvalid"), WebUtil.resetPassword(first, NEW, NEW));
		assertNull(WebUtil.resetPassword(second, NEW, NEW));
	}

	@Test
	void anUnknownEmailAddressGetsNoMailAndNoError() throws Exception {
		WebUtil.requestPasswordReset(CUSTOMER, "nobody-" + System.nanoTime() + "@skyve.org");

		assertEquals(0, capture.getSent().size());
	}

	@Test
	void everyUserWithTheEmailAddressGetsTheSameTokenAndOneMailIsSent() throws Exception {
		Fixture a = user("shared-a", Boolean.FALSE, null);
		Fixture b = user("shared-b", Boolean.FALSE, null);
		String email = "shared-" + System.nanoTime() + "@skyve.org";
		setEmail(a.name, email);
		setEmail(b.name, email);

		WebUtil.requestPasswordReset(CUSTOMER, email);

		assertNotNull(tokenOf(a.name));
		assertEquals(tokenOf(a.name), tokenOf(b.name));
		assertEquals(1, capture.getSent().size());
	}

	private record Fixture(String name, String token) {
		// the user's name and its reset token
	}

	/**
	 * Saves a user with a reset token (null timestamp: no token).
	 */
	private static Fixture user(String prefix, Boolean inactive, Timestamp tokenCreated) {
		String name = "reset." + prefix + "." + System.nanoTime();
		String token = "token-" + name;
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName(name);
		user.setInactive(inactive);
		user.setPassword(EXT.hashPassword(OLD));
		user.setPasswordHistory(null);
		user.setPasswordResetToken((tokenCreated == null) ? null : token);
		user.setPasswordResetTokenCreationTimestamp(tokenCreated);
		user.getContact().setEmail1(name + "@skyve.org");
		user.getGroups().clear();
		user.getRoles().clear();
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.BasicUser");
		user.addRolesElement(role);
		CORE.getPersistence().save(user);
		CORE.getPersistence().commit(false);
		CORE.getPersistence().begin();
		return new Fixture(name, token);
	}

	private static String passwordHash(String name) throws Exception {
		return scalar("select password from ADM_SecurityUser where bizCustomer = ? and userName = ?", name);
	}

	private static String tokenOf(String name) throws Exception {
		return scalar("select passwordResetToken from ADM_SecurityUser where bizCustomer = ? and userName = ?", name);
	}

	private static String emailOf(String name) throws Exception {
		return scalar("select c.email1 from ADM_SecurityUser u inner join ADM_Contact c on u.contact_id = c.bizId "
				+ "where u.bizCustomer = ? and u.userName = ?", name);
	}

	private static String scalar(String sql, String name) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(); PreparedStatement ps = c.prepareStatement(sql)) {
			ps.setString(1, CUSTOMER);
			ps.setString(2, name);
			try (ResultSet rs = ps.executeQuery()) {
				assertTrue(rs.next());
				return rs.getString(1);
			}
		}
	}

	private static void setEmail(String name, String email) throws Exception {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement ps = c.prepareStatement(
						"update ADM_Contact set email1 = ? where bizId = (select contact_id from ADM_SecurityUser where bizCustomer = ? and userName = ?)")) {
			c.setAutoCommit(true);
			ps.setString(1, email);
			ps.setString(2, CUSTOMER);
			ps.setString(3, name);
			assertEquals(1, ps.executeUpdate());
		}
	}

	/**
	 * Sets the token expiry on the configuration (creating one if there is none), or clears it.
	 */
	private static void setExpiryMinutes(Integer minutes) throws Exception {
		if (updateExpiry(minutes) == 0 && minutes != null) {
			ConfigurationExtension configuration = new DataBuilder().fixture(FixtureType.crud)
					.build(Configuration.MODULE_NAME,
							Configuration.DOCUMENT_NAME);
			configuration.getStartup().setMapLayer(StartupBizlet.MAP_LAYER_GMAP);
			configuration.getStartup().setMailPort(Integer.valueOf(25));
			configuration.setPasswordMinLength(Integer.valueOf(ConfigurationExtension.PASSWORD_DEFAULT_MIN_LENGTH));
			CORE.getPersistence().save(configuration);
			CORE.getPersistence().commit(false);
			CORE.getPersistence().begin();
			updateExpiry(minutes);
		}
	}

	private static int updateExpiry(Integer minutes) throws Exception {
		try (Connection c = EXT.getDataStoreConnection(); Statement s = c.createStatement()) {
			c.setAutoCommit(true);
			return s.executeUpdate("update ADM_Configuration set passwordResetTokenExpiryMinutes = " + minutes);
		}
	}
}
